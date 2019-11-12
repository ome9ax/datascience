#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# setwd('~/projects/training/datascience/data_products/house')

required.packages <- c('doParallel', 'caret', 'shiny', 'leaflet') # 'ggplot2', 'beepr', 'pgmm', 'knitr', 'MASS', 'forecast', 'maps', 'maptools', 'sp', 'rgeos'
missing.packages <- setdiff(required.packages, rownames(installed.packages()))
if (length(missing.packages)) install.packages(missing.packages)
sapply(required.packages, require, character.only = TRUE)

if (length(setdiff(c('arrow'), rownames(installed.packages())))) devtools::install_github('apache/arrow/r')
library(arrow)

registerDoParallel(cores = ifelse(detectCores() > 1, detectCores() - 1, 1))
getDoParWorkers()

# mapStates <- map('state', fill = TRUE, plot = FALSE)
load_data <- function(file_url, data_dir = 'data', cache = FALSE){
  file_path <- file.path(data_dir, basename(file_url))
  as_tibble(transform(
    read.csv(file_path),
    # id = as.character(id),
    zipcode = factor(zipcode),
    date = as.integer(substring(as.character(date), 1, 8)),
    year = as.integer(substring(as.character(date), 1, 4)),
    month = as.integer(substring(as.character(date), 5, 6)),
    week = as.integer(format(as.Date(substring(as.character(date), 1, 8), format = '%Y%m%d'), '%W'))
  ))
}

raw_house <- load_data('kc_house_data.csv.xz', 'data')
# house <- raw_house[sample(nrow(raw_house), 9999), -nearZeroVar(raw_house)]
house <- raw_house[, -nearZeroVar(raw_house)]

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

house.price.mean <- price.mean(house)

# My 1st aborted approach was to split the data between 2014 and 2015
# This split is pretty weak because it ignore the seasonality effects on the transactions.
# This dataset doesn't contains a wide enough time periode to be reliable across 2 years.
# Those are likely influencal as it would make sense the people would be less likely to quite their house during winter that in summer for instance.
# So a classic fold partitioning is choosen instead
set.seed(99)
dataPartitions <- createDataPartition(y = house$price, p = 0.7, list = F)
training <- house[dataPartitions, ]
validation <- house[-dataPartitions, ]

# Multiple predictors linear model
modLm <- lm(price ~ zipcode + grade + condition + sqft_lot + bedrooms + week, training)
# modLm <- lm(price ~ . - id - date - year - lat - long + grade, training)
modLmStep <- stepAIC(modLm, direction = 'both', trace = FALSE)
# stepAIC got rid of id lat and week
predLm <- predict(modLmStep, validation)
accLm <- accuracy(predLm, validation$price)

# GBM boost with tress ML model
trControl <- trainControl(method = 'repeatedcv', number = 6, repeats = 3)
# trControl <- trainControl(method = 'cv', number = 8, allowParallel = TRUE, verboseIter = FALSE)
# trControl <- trainControl(method = 'boot', number = 1, allowParallel = TRUE)
modGbm <- train(price ~ zipcode + grade + condition + sqft_lot + bedrooms + week, data = training, method = 'gbm', trControl = trControl, verbose = F) # NOTE : gbm boost with trees
predGbm <- predict(modGbm, validation)
accGbm <- accuracy(predGbm, validation$price)

validation.price.mean <- merge(
  price.mean(validation),
  # aggregate(price.lm ~ zipcode, cbind(validation[, c('id', 'zipcode')], price.lm = predLm), mean),
  aggregate(cbind(price.lm, price.gbm) ~ zipcode, cbind(validation[, c('id', 'zipcode')], price.lm = predLm, price.gbm = predGbm), mean),
  by = 'zipcode'
)

# First design
ui <- fluidPage(

  # Application title
  titlePanel('Predict house price'),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput('price',
                  'Subset by price:',
                  min = min(training$price),
                  max = max(training$price),
                  value = range(training$price)),
      selectInput('zipcode',
                  'Zipcode:',
                  choices = levels(house$zipcode)),
      sliderInput('grade',
                  'Grade:',
                  min = min(house$grade),
                  max = max(house$grade),
                  step = 1,
                  value = round(median(house$grade))),
      sliderInput('condition',
                  'Condition:',
                  min = min(house$condition),
                  max = max(house$condition),
                  step = 1,
                  value = round(median(house$condition))),
      sliderInput('sqft_lot',
                  'Sqrt ft:',
                  min = min(house$sqft_lot),
                  max = max(house$sqft_lot),
                  step = 1,
                  value = round(median(house$sqft_lot))),
      sliderInput('bedrooms',
                  'Bedrooms:',
                  min = min(house$bedrooms),
                  max = max(house$bedrooms),
                  step = 1,
                  value = round(median(house$bedrooms))),
      sliderInput('week',
                  'Week:',
                  min = min(house$week),
                  max = max(house$week),
                  step = 1,
                  value = round(median(house$week)))
      # h5('House count:'),
      # textOutput('house.count'),
      # NOTE : manually trigger the calculation if too slow
      # submitButton('Execute')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(# type = 'pills',
        tabPanel(h4('Average prices/zipcode'),# br(),
                 leafletOutput('map'),
                 plotOutput('distPrice', height = '250px')
        ),
        tabPanel(h4('Predictions'),# br(),0
                 h5('Multiple predictors linear model price prediction: '),
                 textOutput('price.predLm'),
                 h5('GBM boost with tress ML model price prediction: '),
                 textOutput('price.predGbm'),
                 plotOutput('predPlot', brush = brushOpts(id = 'brush.subset'))
        )
      )
    )
  )
)

# # Other UI design inspired from the shiny examples : https://shiny.rstudio.com/gallery/superzip-example.html
# ui <- navbarPage('Predict house price', id = 'nav',
#   tabPanel('Average prices/zipcode',
#     div(class = 'outer',
#       tags$head(
#         includeCSS('styles.css'),
#         includeScript('gomap.js')
#       ),
#       leafletOutput('map', width = '100%', height = '100%'),
#       absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
#                     draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
#                     width = 330, height = 'auto',
#
#         h2('Map explorer'),
#         h4('Filter'),
#         sliderInput('price',
#                     'Subset by price:',
#                     # step = 10,
#                     min = min(training$price),
#                     max = max(training$price),
#                     value = range(training$price)),
#         # h5('House count:'),
#         # textOutput('house.count'),
#         # NOTE : manually trigger the calculation if too slow
#         # submitButton('Execute')
#         plotOutput('distPrice', height = '250px')
#         # NOTE : manually trigger the calculation if too slow
#         # submitButton('Execute')
#         ),
#       tags$div(id = 'cite',
#                'Data compiled for ',
#                tags$em('House Sales in  King County, Washington State, USA, 2014–2015'),
#                ' from ',
#                a('Kaggle', href = 'https://www.kaggle.com/harlfoxem/housesalesprediction')
#                )
#     )
#   ),
#   tabPanel('Predictions',
#     sidebarPanel(
#     h4('Prediction parameters'),
#       # sliderInput('pred.price',
#       #             'Subset by price:',
#       #             min = min(training$price),
#       #             max = max(training$price),
#       #             value = range(training$price)),
#       selectInput('zipcode',
#                  'Zipcode:',
#                  choices = levels(training$zipcode)),
#       sliderInput('sqft_lot',
#                  'Sqrt ft:',
#                  min = min(training$sqft_lot),
#                  max = max(training$sqft_lot),
#                  step = 1,
#                  value = round(median(training$sqft_lot))),
#       sliderInput('bedrooms',
#                  'Bedrooms:',
#                  min = min(training$bedrooms),
#                  max = max(training$bedrooms),
#                  step = 1,
#                  value = round(median(training$bedrooms))),
#       sliderInput('week',
#                  'Week:',
#                  min = min(training$week),
#                  max = max(training$week),
#                  step = 1,
#                  value = round(median(training$week)))
#     ),
#     mainPanel(
#       h4('Multiple predictors linear model price prediction: '),
#       textOutput('price.predLm'),
#       h4('GBM boost with tress ML model price prediction: '),
#       textOutput('price.predGbm'),
#       plotOutput('predPlot')
#     )
#   )
#   # titlePanel('Predict house price'),
# )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # values <- reactiveValues(starting = TRUE)
  # session$onFlushed(function() {
  #   values$starting <- FALSE
  # })

  price.subset <- function(dataset, input) dataset[dataset$price >= input$price[1] & dataset$price <= input$price[2], ]

  dataset.custom <- function(dataset, input, model) {
    dataset <- dataset[dataset$zipcode == input$zipcode, c('grade', 'condition', 'zipcode', 'sqft_lot', 'bedrooms', 'week')][1, ]
    dataset[, c('grade', 'condition', 'sqft_lot', 'bedrooms', 'week')] <- c(input$grade, input$condition, input$sqft_lot, input$bedrooms, input$week)
    cbind(dataset, price = predict(model, newdata = dataset))
  }

  house.react <- reactive({
    price.subset(house, input)
  })

  price.mean.react <- reactive({
    price.subset(house.price.mean, input)
  })

  validation.react <- reactive({
    brushed.subset <- brushedPoints(validation.price.mean, input$brush.subset, xvar = 'zipcode', yvar = 'price')
    if (nrow(brushed.subset) > 1)
      validation.price.mean[validation.price.mean$zipcode %in% brushed.subset$zipcode & validation.price.mean$price %in% brushed.subset$price, ]
    else
      validation.price.mean
    # price.subset(validation.price.mean, input)
  })

  predLm.react <- reactive({
    dataset.custom(validation, input, modLmStep)
  })

  predGbm.react <- reactive({
    dataset.custom(validation, input, modGbm)
  })

  output$distPrice <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- house.react()$price
    bins <- seq(min(x), max(x), length.out = ifelse(length(x) > 30, 30, length(x)))

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = 'Price')
  })

  output$map <- renderLeaflet({
    # if (values$starting) return(NULL)

    house.subset <- house.react()

    price.mean.subset <- price.mean.react()

    house.subset %>%
      leaflet() %>%
      addTiles(options = tileOptions(detectRetina = TRUE)) %>%
      addMarkers(popup = paste0('$', house.subset$price), clusterOptions = markerClusterOptions(), options = markerOptions()) %>%
      # addCircleMarkers(radius = 10 * house$price.rescale, color = house$price.color, opacity = 0.5, fillOpacity = 0.5)
      addCircles(lat = price.mean.subset$lat, lng = price.mean.subset$long, weight = 3, radius = price.mean.subset$count * 5, color = price.mean.subset$price.color, opacity = 0.5, fillOpacity = 0.5) %>%
      addLegend(title = 'Mean price', labels = seq(max(house$price), min(house$price), len = 9), colors = colorRampPalette(c('#DB4A46', '#F0AD4E', '#59BB59'))(9), position = 'bottomright')
  })

  output$price.predLm <- renderText({ paste0('$ ', round(predLm.react()$price)) })

  output$price.predGbm <- renderText({ paste0('$ ', round(predGbm.react()$price)) })

  output$predPlot <- renderPlot({
    validation.subset <- validation.react()
    # validation.subset <- validation.price.mean
    predLm.subset <- predLm.react()
    predGbm.subset <- predGbm.react()
    if(nrow(validation.subset)){
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
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
