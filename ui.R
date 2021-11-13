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
        useShinyjs(),
        column(width=3,
               align="left",
               fileInput("files", "Choose PNG Files", accept = ".png", multiple=TRUE),
               rHandsontableOutput("hot_files"),
               tags$p(""),
               wellPanel(id="selection_well", 
                         tags$div(id="numericinput_selection",
                                  numericInput('size', 'Pixel size', 500, min = 10, max = 1000),
                                  numericInput('coord.x', 'X Coordinate (top left)', 1, min = 1),
                                  numericInput('coord.y', 'Y coordinate (top left)', 1, min = 1),
                         ),
                         hidden(
                           plotOutput("plot_overview", width="400px", height="260px", click = "img_click")
                         )
               ),
               wellPanel(id="composite_well",
                         checkboxInput("check_gap", "Insert a gap", TRUE),
                         conditionalPanel(
                           condition = "input.check_gap == 1",
                           tags$div(id = "div_gap", class="x", numericInput('gap', 'Gap size:', 15, min = 0, max = 100)),
                         ) # end conditionalPanel
                         ),
               rHandsontableOutput("hot_colors"),
               wellPanel(id="scalebar_well",
                         checkboxInput("check_scalebar", "Add a scale bar", FALSE),
                         conditionalPanel(
                           condition = "input.check_scalebar == 1",
                           tags$div(id = "div_scalebar", class="x",
                                    selectInput("select_objective", "Objective:", choices = c("5x", "10x", "20x", "40x", "Other")),
                                    numericInput("px_per_um", "Pixels per µm:", 100, min = 1))
                         )),
               tags$p(""),
               hidden(downloadButton("download", "Download composite"))
        ), # end column
        column(width=9,
               hidden(
                 tags$div(id="div_plot_originals",
                          tags$h3("Original:"),
                          plotOutput("plot1")
                 ) # end tags$div
               ), # end hidden
               hidden(
                 tags$div(id="div_plot_autocontrast",
                          tags$h3("Autocontrast:"),
                          plotOutput("plot2")
                 ) # end tags$div
               ) # end hidden
               
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

