#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- navbarPage('Predict house sales', id = 'nav',
  tabPanel('Average prices/zipcode',
    # div(class = 'outer',
    #     tags$head(
    #       includeCSS('styles.css')
    #       # includeScript('gomap.js')
    #     ),
    sidebarPanel(

      h2('Map explorer'),
      h4('Filter'),
      htmlOutput('price'),
      # h5('House count:'),
      # textOutput('house.count'),
      # NOTE : manually trigger the calculation if too slow
      # submitButton('Execute')
      plotOutput('distPrice', height = '250px'),
      h6('NOTE : the circle radius is the count of sales, and the color the average prices, both per zipcode.'),
      h6('INFO : Available calculation units :'),
      textOutput('workers')
      # NOTE : manually trigger the calculation if too slow
      # submitButton('Execute')
    ),
    mainPanel(leafletOutput('map'))
    #     tags$div(id = 'cite',
    #              'Data compiled for ',
    #              tags$em('House Sales in  King County, Washington State, USA, 2014–2015'),
    #              ' from ',
    #              a('Kaggle', href = 'https://www.kaggle.com/harlfoxem/housesalesprediction')
    #     )
    # )
  ),
  tabPanel('Predictions',
    sidebarPanel(
      h4('Prediction parameters'),
      htmlOutput('zipcode'),
      htmlOutput('grade'),
      htmlOutput('condition'),
      htmlOutput('sqft_lot'),
      htmlOutput('bedrooms'),
      htmlOutput('week')
    ),
    mainPanel(
      h4('Multiple predictors linear model price prediction: '),
      textOutput('price.predLm'),
      h4('GBM boost with tress ML model price prediction: '),
      textOutput('price.predGbm'),
      plotOutput('predPlot', brush = brushOpts(id = 'brush.subset')),
      h6('NOTE : brush the chart with your screen pointer to subset the displayed aggregates')
    )
  )
)

# Better design but too slow. Need to be optimized
ui.dev <- navbarPage('Predict house sales', id = 'nav',
                     tabPanel('Average prices/zipcode',
                              div(class = 'outer',
                                  tags$head(
                                    includeCSS('styles.css')
                                    # includeScript('gomap.js')
                                  ),
                                  leafletOutput('map', width = '100%', height = '100%'),
                                  absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
                                                draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
                                                width = 330, height = 'auto',

                                                h2('Map explorer'),
                                                h4('Filter'),
                                                htmlOutput('price'),
                                                # h5('House count:'),
                                                # textOutput('house.count'),
                                                # NOTE : manually trigger the calculation if too slow
                                                # submitButton('Execute')
                                                plotOutput('distPrice', height = '250px'),
                                                h6('NOTE : the circle radius is the count of sales, and the color the average prices, both per zipcode.'),
                                                h6('INFO : Available calculation units :'),
                                                textOutput('workers')
                                                # NOTE : manually trigger the calculation if too slow
                                                # submitButton('Execute')
                                  ),
                                  tags$div(id = 'cite',
                                           'Data compiled for ',
                                           tags$em('House Sales in  King County, Washington State, USA, 2014–2015'),
                                           ' from ',
                                           a('Kaggle', href = 'https://www.kaggle.com/harlfoxem/housesalesprediction')
                                  )
                              )
                     ),
                     tabPanel('Predictions',
                              sidebarPanel(
                                h4('Prediction parameters'),
                                htmlOutput('zipcode'),
                                htmlOutput('grade'),
                                htmlOutput('condition'),
                                htmlOutput('sqft_lot'),
                                htmlOutput('bedrooms'),
                                htmlOutput('week')
                              ),
                              mainPanel(
                                h4('Multiple predictors linear model price prediction: '),
                                textOutput('price.predLm'),
                                h4('GBM boost with tress ML model price prediction: '),
                                textOutput('price.predGbm'),
                                plotOutput('predPlot', brush = brushOpts(id = 'brush.subset')),
                                h6('NOTE : brush the chart with your screen pointer to subset the displayed aggregates')
                              )
                     )
)

shinyUI(ui)
