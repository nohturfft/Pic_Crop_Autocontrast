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
library(shinyBS)


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
               wellPanel(id="selection_well", 
                         tags$div(id="numericinput_selection",
                                  numericInput('size', 'Pixel size selction', 500, min = 10, max = 1000),
                                  numericInput('coord.x', 'X Coordinate (top left)', 1, min = 1),
                                  numericInput('coord.y', 'Y coordinate (top left)', 1, min = 1),
                         ),
                         hidden(
                           plotOutput("plot_overview", width="400px", height="260px", click = "img_click")
                         ),
                         hidden(
                           radioButtons(inputId = "radio_overview", label = NULL, choices = c("Pic1", "Pic2", "Pic3"), selected=1, inline = TRUE)
                         )
               ),
               wellPanel(id="composite_well",
                         splitLayout(
                           checkboxInput("check_gap", "Insert a gap", TRUE),
                           conditionalPanel(
                             condition = "input.check_gap == 1",
                             tags$div(id = "div_gap", class="x", numericInput('gap', 'Size:', 15, min = 0, max = 100)),
                           ) # end conditionalPanel
                         ) # end splitLayout
                         
               ),
               rHandsontableOutput("hot_colors"),
               wellPanel(id="scalebar_well",
                         splitLayout(
                         checkboxInput("check_scalebar", "Add a scale bar", TRUE),
                         conditionalPanel(
                           condition = "input.check_scalebar == 1",
                           actionButton("show_scalebar_modal", "Options"),
                           
                           bsModal("params", "Options", "show_scalebar_modal", size = "small",
                                   tags$div(id = "div_scalebar", class="x",
                                            selectInput("scalebar_objective", "Objective:", choices = c("5x", "10x", "20x", "40x", "Other"),
                                                        selected="20x"),
                                            numericInput("scalebar_px_per_um", "Pixels per µm:", 3.424),
                                            numericInput("scalebar_microns", "Scalebar length (µm):", 100, min = 1),
                                            numericInput("scalebar_height", "Bar height (px):", 20, min = 10, max=100),
                                            numericInput("scalebar_txt_height", "Text height (px):", 20, min = 6, max=100),
                                            numericInput("scalebar_padding", "Padding (px):", 10, min = 0),
                                            numericInput("scalebar_offset", "Offset (px):", 0, min = 0),
                                            selectInput("scalebar_color", "Colour:", choices = c("white", "black"), selected="black")
                                   ) # end tags$div
                           ) # end bsModal
                           
                         ) # end conditionalPanel
                         ) # end splitLayout
                         ), # end wellPanel
               tags$p(""),
               hidden(downloadButton("download", "Download composite"))
        ), # end column
        column(width=9,
               bsCollapse(
                 id = "collapseExample", open = "Autocontrast", multiple = TRUE,
                 bsCollapsePanel("Originals",
                                 hidden(
                                   tags$div(id="div_plot_originals",
                                            plotOutput("plot1")
                                   ) # end tags$div
                                 ), # end hidden
                                 style="info"
                 ), # end bsCollapsePanel
                 bsCollapsePanel("Autocontrast", 
                                 hidden(
                                   tags$div(id="div_plot_autocontrast",
                                            plotOutput("plot2")
                                   ) # end tags$div
                                 ), # end hidden
                                 style = "primary"
                 ) # end bsCollapsePanel
               ) # end bsCollapse 
        ) # end column
      ) # fluidPage
    ), # end TabPanel
    tabPanel(
      title = "Quit",
      value="stop",
      icon = icon("circle-o-notch")
    ) # end tabPanel "quit"
  ) # end navbarPage
) # end ShinyUI

