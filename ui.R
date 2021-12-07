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
               hidden(
                 tags$div(id="file_count_error", class="error_msg", tags$p("Error. Please choose at least 2 files."))
               ),
               rHandsontableOutput("hot_files"),
               wellPanel(id="selection_well", 
                         tags$div(id="numericinput_selection",
                                  numericInput('size', 'Pixel size selection', 650, min = 10, max = 1000),
                                  numericInput('coord.x', 'X Coordinate (top left)', 1, min = 1),
                                  numericInput('coord.y', 'Y coordinate (top left)', 1, min = 1),
                         ),
                         actionButton("selection_btn", "Apply changes"),
                         hidden(
                           plotOutput("plot_overview", width="400px", height="260px", click = "img_click")
                         ), # end hidden
                         hidden(
                           radioButtons(inputId = "radio_overview", label = NULL, choices = c("Pic1", "Pic2", "Pic3"), selected=1, inline = TRUE)
                         ) # end hidden
               ), # end wellPanel
               
               wellPanel(id="montage_well",
                         splitLayout(cellWidths = c("80%", "20%"),
                           numericInput("montage_max_width", "Montage max width (px):", 850),
                           actionButton("apply_max_width", "Apply")
                         ),
                         splitLayout(
                           checkboxInput("check_gap", "Insert a gap", TRUE),
                           conditionalPanel(
                             condition = "input.check_gap == 1",
                             # tags$div(id = "div_gap", class="x", numericInput('gap', 'Gap Size (px):', 15, min = 0, max = 100)),
                             numericInput('gap', 'Gap Size (px):', 15, min = 0, max = 100)
                           ) # end conditionalPanel
                         ), # end splitLayout
                         splitLayout(
                           # Scalebar checkbox ####
                           checkboxInput("check_scalebar", "Add scale bar + info", TRUE),
                           conditionalPanel(
                             condition = "input.check_scalebar == 1",
                             actionButton("show_scalebar_modal", "Scalebar options"),
                             
                             bsModal("params", "Scalebar options", "show_scalebar_modal", size = "small",
                                     tags$div(id = "div_scalebar", class="x",
                                              selectInput("scalebar_objective", "Objective:", choices = c("5x", "10x", "20x", "40x", "Other"),
                                                          selected="20x"),
                                              numericInput("scalebar_px_per_um", "Pixels per µm:", 3.424),
                                              numericInput("scalebar_microns", "Scalebar length (µm):", 100, min = 1),
                                              numericInput("scalebar_height", "Bar height (px):", 20, min = 10, max=100),
                                              numericInput("scalebar_txt_height", "Text height (px):", 14, min = 6, max=100),
                                              numericInput("scalebar_padding", "Padding (px):", 10, min = 0),
                                              numericInput("scalebar_offset", "Offset (px):", 0, min = 0),
                                              selectInput("scalebar_color", "Colour:", choices = c("white", "black"), selected="black")
                                     ) # end tags$div
                             ) # end bsModal
                           ) # end conditionalPanel
                         ) # end splitLayout
               ), # end wellPanel
               
               rHandsontableOutput("hot_colors"),
               
               # tags$p(""),
               hidden(
                 wellPanel(
                   id="correlation_panel",
                   checkboxInput("check_correl", "Calculate correlation", FALSE),
                   conditionalPanel(
                     condition = "input.check_correl == 1",
                     rHandsontableOutput("hot_correl_files"),
                     tableOutput("table_correl")
                   ) # end conditionalPanel
                 ) # end wellPanel
               ), # end hidden
               hidden(downloadButton("download_composite", "Download composite")),
               hidden(actionButton("download_pics", "Download individual imgs"))
        ), # end column
        
        column(width=9,
               bsCollapse(
                 id = "collapseExample", open = "Autocontrast", multiple = FALSE,
                 bsCollapsePanel("Originals",
                                 hidden(
                                   tags$div(id="div_plot_originals",
                                            plotOutput("plot_originals")
                                   ) # end tags$div
                                 ), # end hidden
                                 style="info"
                 ), # end bsCollapsePanel
                 bsCollapsePanel("Autocontrast", 
                                 hidden(
                                   tags$div(id="div_plot_autocontrast",
                                            plotOutput("plot_montage")
                                   ) # end tags$div
                                 ), # end hidden
                                 style = "primary"
                 ) # end bsCollapsePanel
               ), # end bsCollapse 
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

# undebug("shinyUI")
