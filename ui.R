library(shiny)
library(rhandsontable)
library(shinyjs)

shinyUI(
  navbarPage(
    id="navbar",
    title = "Crop and Autocontrast",
    tabPanel(
      id = "tabpanel_1",
      title = "Main",
      fluidPage(
        column(width=4,
               fileInput("files", "Choose PNG Files", accept = ".png", multiple=TRUE),
               numericInput('size', 'Pixel size', 500, min = 10, max = 1000),
               numericInput('coord.x', 'X Coordinate (top left)', 1, min = 1),
               numericInput('coord.y', 'Y coordinate (top left)', 1, min = 1),
               checkboxInput("check_gap", "Insert a gap", TRUE),
               conditionalPanel(
                 condition = "input.check_gap == 1",
                 tags$head(
                   tags$style(type="text/css",
                              "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                #inline .form-group { display: table-row;}")
                 ),
                tags$div(id = "inline", class="x", numericInput('gap', 'Gap size:', 15, min = 0, max = 100))
               )
               
        ), # end column
        column(width=8,
               plotOutput("plot1"),
               plotOutput("plot2")
        ) # end column
      ) # end fluidPage
    ), # end TabPanel
    tabPanel(
      title = "Quit",
      value="stop",
      icon = icon("circle-o-notch")
    ) # end tabPanel "quit"
  ) # end navbarPage
) # end ShinyUI

