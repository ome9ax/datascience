#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Old Faithful Geyser Data"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application
# shinyApp(ui = ui, server = server)

library(miniUI)
tryGadget <- function(){
  ui <- miniPage(
    gadgetTitleBar('Try Gadget')
  )
  server <- function(input, output, session){
    # Done button close the app
    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server)
}

multiplyNumbers <- function(number1, number2){
  ui <- miniPage(
    gadgetTitleBar('Multiply two numbers'),
    miniContentPanel(
      selectInput('num1', 'First number', choices = number1),
      selectInput('num2', 'Second number', choices = number2)
    )
  )
  server <- function(input, output, session){
    observeEvent(input$done, {
      num1 <- as.numeric(input$num1)
      num2 <- as.numeric(input$num2)
      stopApp(num1 * num2)
    })
  }
  runGadget(ui, server)
}

pickTrees <- function(){
  ui <- miniPage(
    gadgetTitleBar('Select point by dragging your pointer'),
    miniContentPanel(
      plotOutput('plot', height = '100%', brush = 'brush')
    )
  )
  server <- function(input, output, session){
    output$plot <- renderPlot({
      plot(trees$Girth, trees$Volume, main = 'Trees !', xlab = 'Girth', ylab = 'Volume')
    })
    observeEvent(input$done, {
      stopApp(brushedPoints(trees, input$brush, xvar = 'Girth', yvar = 'Volume'))
    })
  }
  runGadget(ui, server)
}

library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)
