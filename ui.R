#-------------------------------------------------------------------------------!
library(BiocManager)
options(repos = BiocManager::repositories())
library(magick)
library(EBImage)
library(shiny)
library(rhandsontable)
library(shinyjs)
library(purrr)
library(shiny)
library(imager)


shinyUI(
  navbarPage(
    theme = "styles.css",
    id="navbar",
    title = "Crop and Autocontrast",
    tabPanel(
      id = "tabpanel_1",
      title = "Main",
      fluidPage(
        column(width=4,
               fileInput("files", "Choose PNG Files", accept = ".png", multiple=TRUE),
               # imageOutput("original_pic"),
               numericInput('size', 'Pixel size', 500, min = 10, max = 1000),
               numericInput('coord.x', 'X Coordinate (top left)', 1, min = 1),
               numericInput('coord.y', 'Y coordinate (top left)', 1, min = 1),
               checkboxInput("check_gap", "Insert a gap", TRUE),
               conditionalPanel(
                 condition = "input.check_gap == 1",
                tags$div(id = "inline", class="x", numericInput('gap', 'Gap size:', 15, min = 0, max = 100)),
               ), # end conditionalPanel
               rHandsontableOutput("hot"),
               tags$p(""),
               downloadButton("download", "Download composite")
        ), # end column
        column(width=8,
               # displayOutput("plot3"),
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

