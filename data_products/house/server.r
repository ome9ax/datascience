#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd('~/projects/training/datascience/data_products/house')

# required.packages <- c('doParallel', 'shiny', 'leaflet', 'caret', 'MASS', 'gbm', 'forecast') # 'devtools', 'ggplot2', 'beepr', 'pgmm', 'knitr', 'maps', 'maptools', 'sp', 'rgeos'
# missing.packages <- setdiff(required.packages, rownames(installed.packages()))
# if (length(missing.packages)) install.packages(missing.packages)
# sapply(required.packages, library, character.only = TRUE)
# if (length(setdiff(c('arrow'), rownames(installed.packages())))) devtools::install_github('apache/arrow/r')
# library(arrow)

library(doParallel)
library(shiny)
library(leaflet)
library(caret)
library(MASS)
library(gbm)
library(forecast)
# install.packages('MonetDBLite')
library(DBI)
library(dplyr)

registerDoParallel(cores = ifelse(detectCores() > 1, detectCores() - 1, 1))

# mapStates <- map('state', fill = TRUE, plot = FALSE)
load_data <- function(file_url, data_dir = 'data', cache = FALSE){
  file_path <- file.path(data_dir, basename(file_url))
  # as_tibble(
    transform(
      read.csv(file_path),
      # id = as.character(id),
      zipcode = factor(zipcode),
      date = as.integer(substring(as.character(date), 1, 8)),
      year = as.integer(substring(as.character(date), 1, 4)),
      month = as.integer(substring(as.character(date), 5, 6)),
      week = as.integer(format(as.Date(substring(as.character(date), 1, 8), format = '%Y%m%d'), '%W'))
    )
  # )
}

raw_house <- load_data('kc_house_data.csv.xz', './data')
# house <- raw_house[sample(nrow(raw_house), 5000), -nearZeroVar(raw_house)]
house <- raw_house[, -nearZeroVar(raw_house)]

rm(list = c('load_data', 'raw_house'))

# My 1st aborted approach was to split the data between 2014 and 2015
# This split is pretty weak because it ignore the seasonality effects on the transactions.
# This dataset doesn't contains a wide enough time periode to be reliable across 2 years.
# Those are likely influencal as it would make sense the people would be less likely to quite their house during winter that in summer for instance.
# So a classic fold partitioning is choosen instead
set.seed(99)
dataPartitions <- createDataPartition(y = house$price, p = 0.8, list = F)
training <- house[dataPartitions, ]
validation <- house[-dataPartitions, ]

# con <- dbConnect(MonetDBLite::MonetDBLite())
# dbdir <- file.path(tempdir(), 'dbdir')
# mn_db <- MonetDBLite::src_monetdblite(dbdir)
# training  <- copy_to(mn_db, house[dataPartitions, ], overwrite = T)
# validation  <- copy_to(mn_db, house[-dataPartitions, ], overwrite = T)
# dbDisconnect(con, shutdown = TRUE)
# MonetDBLite::monetdblite_shutdown()

rm(list = c('house', 'dataPartitions'))

# Multiple predictors linear model
modLm <- lm(price ~ zipcode + grade + condition + sqft_lot + bedrooms + week, training)
# modLm <- lm(price ~ . - id - date - year - lat - long + grade, training)
modLmStep <- stepAIC(modLm, direction = 'both', trace = FALSE)
# stepAIC got rid of id lat and week
predLm <- predict(modLmStep, validation)
accLm <- accuracy(predLm, validation$price)

# GBM boost with tress ML model
trControl <- trainControl(method = 'repeatedcv', number = 6, repeats = 3, allowParallel = TRUE, verboseIter = FALSE)
# trControl <- trainControl(method = 'cv', number = 8, allowParallel = TRUE, verboseIter = FALSE)
# trControl <- trainControl(method = 'boot', number = 1, allowParallel = TRUE)
modGbm <- train(price ~ zipcode + grade + condition + sqft_lot + bedrooms + week, data = training, method = 'gbm', trControl = trControl, verbose = F) # NOTE : gbm boost with trees
predGbm <- predict(modGbm, validation)
accGbm <- accuracy(predGbm, validation$price)

rm(training)

validation.price <- transform(validation, price = as.integer(round(price)), price.gbm = as.integer(round(predGbm)), price.lm = as.integer(round(predLm)))

price.mean <- function(dataset) within(
  cbind(
    aggregate(cbind(lat, long) ~ zipcode, dataset, median),
    price = aggregate(price ~ zipcode, dataset, mean)$price,
    count = aggregate(id ~ zipcode, dataset, length)$id
  ), {
    # price.rescale <- (price - min(price)) / (max(price) - min(price))

    # NOTE : colours following the sqrt of the rescaled value in order to increase the weight of the highest prices
    price.color <- as.character(cut(
      price,
      breaks = seq(min(price), max(price), len = 10),
      labels = colorRampPalette(c('#59BB59', '#F0AD4E', '#DB4A46'))(9),
      include.lowest = TRUE
    ), stringsAsFactors = FALSE)
    # NOTE : colorRamp is the right tool to get a linear association between colours and values
    # price.color <- rgb(colorRamp(c('#59BB59', '#F0AD4E', '#DB4A46'))(price.rescale) / 255)
    # NOTE : colour breaks into tertiles
    # price.color <- as.character(cut(
    #   sqrt(price.rescale),
    #   breaks = c(0, 1.0 / 3, 2.0 / 3, Inf),
    #   labels = c('#59BB59', '#F0AD4E', '#DB4A46')
    # ), stringsAsFactors = FALSE)
  })
# names(price.mean)[names(price.mean) == 'id'] <- 'count'

# house.price.mean <- price.mean(house)

validation.gbm <- price.mean(transform(validation, price = predGbm))
names(validation.gbm)[names(validation.gbm) == 'price'] <- 'price.gbm'

validation.price.mean <- transform(merge(
    validation.gbm,
    aggregate(cbind(price, price.lm) ~ zipcode, cbind(validation[, c('id', 'zipcode', 'price')], price.lm = predLm), mean),
    by = 'zipcode'
  ),
  price = as.integer(round(price)),
  price.gbm = as.integer(round(price.gbm)),
  price.lm = as.integer(round(price.lm))
)

rm(list = c('price.mean', 'validation.gbm'))

server <- function(input, output, session) {

  # values <- reactiveValues(starting = TRUE)
  # session$onFlushed(function() {
  #   values$starting <- FALSE
  # })

  price.subset <- function(dataset, price){
    if(is.null(price)) price <- range(validation$price)
    dataset[dataset$price >= price[1] & dataset$price <= price[2], ]
  }

  dataset.custom <- function(dataset, input, model)
    if(!is.null(input$zipcode)) {
      dataset <- dataset[dataset$zipcode == input$zipcode, c('grade', 'condition', 'zipcode', 'sqft_lot', 'bedrooms', 'week')][1, ]
      dataset[, c('grade', 'condition', 'sqft_lot', 'bedrooms', 'week')] <- c(input$grade, input$condition, input$sqft_lot, input$bedrooms, input$week)
      transform(
        cbind(dataset, price = predict(model, dataset)),
        zipcode = as.numeric(levels(zipcode))[zipcode]
      )
    }

  house.react <- reactive({
    price.subset(validation.price, input$price)
  })

  price.mean.react <- reactive({
    price.subset(validation.price.mean, input$price)
  })

  validation.react <- reactive({
    dataset <- transform(validation.price.mean, zipcode = as.numeric(levels(zipcode))[zipcode])
    brushed.subset <- brushedPoints(dataset, input$brush.subset, xvar = 'zipcode', yvar = 'price')
    if (nrow(brushed.subset))
      dataset[dataset$zipcode %in% brushed.subset$zipcode & dataset$price %in% brushed.subset$price, ]
    else
      dataset
  })

  predLm.react <- reactive({
    dataset.custom(validation, input, modLmStep)
  })

  predGbm.react <- reactive({
    dataset.custom(validation, input, modGbm)
  })

  output$price <- renderUI({
    sliderInput('price',
                'Subset by price:',
                min = min(validation$price),
                max = max(validation$price),
                value = range(validation$price))
  })

  output$zipcode <- renderUI({
    selectInput('zipcode',
                'Zipcode:',
                choices = levels(validation$zipcode))
  })

  output$grade <- renderUI({
    sliderInput('grade',
                'Grade:',
                min = min(validation$grade),
                max = max(validation$grade),
                step = 1,
                value = round(median(validation$grade)))
  })

  output$condition <- renderUI({
    sliderInput('condition',
                'Condition:',
                min = min(validation$condition),
                max = max(validation$condition),
                step = 1,
                value = round(median(validation$condition)))
  })

  output$sqft_lot <- renderUI({
    sliderInput('sqft_lot',
                'Sqrt ft:',
                min = min(validation$sqft_lot),
                max = max(validation$sqft_lot),
                step = 1,
                value = round(median(validation$sqft_lot)))
  })

  output$bedrooms <- renderUI({
    sliderInput('bedrooms',
                'Bedrooms:',
                min = min(validation$bedrooms),
                max = max(validation$bedrooms),
                step = 1,
                value = round(median(validation$bedrooms)))
  })

  output$week <- renderUI({
    sliderInput('week',
                'Week:',
                min = min(validation$week),
                max = max(validation$week),
                step = 1,
                value = round(median(validation$week)))
  })

  output$distPrice <- renderPlot({
    # generate bins based on input$bins from ui.R
    price <- house.react()$price.gbm
    bins <- seq(min(price), max(price), length.out = ifelse(length(price) > 30, 30, length(price)))

    # draw the histogram with the specified number of bins
    hist(price, breaks = bins, col = 'darkgray', border = 'white', xlab = 'Price')
  })

  output$count <- renderText({ nrow(house.react()) })

  output$workers <- renderText({ getDoParWorkers() })

  output$map <- renderLeaflet({
    # if (values$starting) return(NULL)

    house.subset <- house.react()

    house.subset %>%
      leaflet() %>%
      addTiles(options = tileOptions(detectRetina = TRUE)) %>%
      fitBounds(~min(house.subset$long), ~min(house.subset$lat), ~max(house.subset$long), ~max(house.subset$lat))
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({

    house.subset <- house.react()

    price.mean.subset <- transform(price.mean.react(), price = price.gbm)

    price.popup <- paste0('Real price: $ ', house.subset$price, '<br />', 'GBM price: $ ', house.subset$price.gbm, '<br />', 'LM price: $ ', house.subset$price.lm)

    leafletProxy('map', data = house.subset) %>%
      addMarkers(popup = price.popup, clusterOptions = markerClusterOptions(), options = markerOptions()) %>%
      # addCircleMarkers(radius = 10 * house$price.rescale, color = house$price.color, opacity = 0.5, fillOpacity = 0.5)
      addCircles(lat = price.mean.subset$lat, lng = price.mean.subset$long, weight = 4, radius = price.mean.subset$count * 30, color = price.mean.subset$price.color, opacity = 0.5, fillOpacity = 0.5) %>%
      addLegend(title = 'Mean price', labels = seq(max(validation.price$price), min(validation.price$price), len = 9), colors = colorRampPalette(c('#DB4A46', '#F0AD4E', '#59BB59'))(9), position = 'bottomright')
  })

  output$price.predLm <- renderText({
      if(!is.null(input$zipcode)) paste0('$ ', round(predLm.react()$price))
    })

  output$price.predGbm <- renderText({
      if(!is.null(input$zipcode)) paste0('$ ', round(predGbm.react()$price))
    })

  output$predPlot <- renderPlot({
    validation.subset <- validation.react()
    predLm.subset <- predLm.react()
    predGbm.subset <- predGbm.react()

    plot(validation.subset$zipcode, validation.subset$price, main  = 'Prices predictions', xlab = 'zipcode', ylab = 'price', pch = 1)
    points(validation.subset$zipcode, validation.subset$price.gbm, col = 'red', lwd = 2, pch = 5)
    # points(validation.subset$zipcode, validation.subset$price.gbm, col = 'blue', lwd = 2, pch = 5)
    # abline(modGbm, col = 'blue', lwd = 2)
    # points(validation.subset$zipcode, predGbm, col = 'blue', lwd = 2)
    points(predLm.subset$zipcode, predLm.subset$price, col = 'green', lwd = 4, pch = 10)
    abline(predLm.subset$price, 0, col = 'green', lwd = 2)
    points(predGbm.subset$zipcode, predGbm.subset$price, col = 'blue', lwd = 4, pch = 10)
    abline(predGbm.subset$price, 0, col = 'blue', lwd = 2)
    legend('topright', c('Price', 'GBM ML prediction', 'LM custom prediction', 'GBM ML custom prediction'), cex = 0.8, col=c('black', 'red', 'green', 'blue'), pch = c(1, 5, 10, 10))
  })

  output$modLmStep <- renderPrint({ print(summary(modLmStep)) })

}

shinyServer(server)
