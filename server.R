#-------------------------------------------------------------------------------!
# Packages 
#-------------------------------------------------------------------------------!
library(shiny)
library(imager)
library(magrittr)
library(rhandsontable)
# library(EBImage)
requireNamespace("purrr")
requireNamespace("EBImage")

#-------------------------------------------------------------------------------!
# Functions
#-------------------------------------------------------------------------------!
my.rescale <- function(imge) {
  (imge - min(imge)) / (max(imge) - min(imge))
}

my.crop <- function(pic, breite, start.x, start.y) {
  imager::imsub(pic,
                x %inr% c(start.x, start.x + breite - 1),
                y %inr% c(start.y, start.y + breite - 1))
}

make.gap <- function(image.list, gap.width, gap.col="white") {
  imager::imfill(x=gap.width, y=height(image.list[[1]]), z=1, val = gap.col) %>% 
    grayscale(.)
}

intersperse.imgs <- function(pic1, pic2, pic.gap) {
  list(pic1, pic.gap, pic2)
}

merge.pics <- function(pic.list, pic.gap) {
  aa <- purrr::reduce(pic.list, intersperse.imgs, pic.gap=pic.gap)
  bb <- lapply(aa, function(y) {if (class(y)!="list") list(y) else y})
  cc <- purrr::flatten(bb)
  stopifnot(class(cc)=="list")
  imager::imappend(cc, "x")
}

# EBImage::as.Image(tmp.1)
# EBImage::toRGB(x)
compose.pics <- function(pic.list, pic.gap) {
  aa <- purrr::reduce(pic.list, intersperse.imgs, pic.gap=pic.gap)
  bb <- lapply(aa, function(y) {if (class(y)!="list") list(y) else y})
  cc <- purrr::flatten(bb)
  dd <- lapply(cc, EBImage::as.Image)
  ee <- lapply(dd, EBImage::toRGB)
  EBImage::abind(ee, along=1)
}

my.false.colorise <- function(bild, farbe) {
  farbe <- tolower(farbe)
  stopifnot(farbe %in% c("red", "green", "blue", "grayscale"))
  
  bild.rescaled <- my.rescale(bild)
  
  if (farbe == "red") {
    pic <- EBImage::rgbImage(red=bild.rescaled)
  } else if (farbe == "green") {
    pic <- EBImage::rgbImage(green=bild.rescaled)
  } else if (farbe == "blue") {
    pic <- EBImage::rgbImage(blue=bild.rescaled)
  } else if (farbe == "grayscale") {
    pic <- bild.rescaled
  }
  pic
}

color.choices <- c("Grayscale", "Green", "Blue", "Red")

#-------------------------------------------------------------------------------!
# Server code
#-------------------------------------------------------------------------------!
shinyServer(function(input, output, server, session) {
  rv <- reactiveValues(
    files = NULL,
    composite.original = NULL,
    composite.rescaled = NULL,
    crop.x = 1,
    crop.y = 1,
    crop.size = 500,
    gap.size = 15,
    img.list = NULL,
    img.list.crop = NULL,
    img.list.crop.rescale = NULL,
    color.list = NULL
    # tmp = NULL
  )
  
  observeEvent(input$files, {
    # browser()
    print("observeEvent(input$files)")
    rv$files <- input$files # data frame
    rv$color.list <- rep("grayscale", nrow(rv$files))
    
    output$hot <- renderRHandsontable({
      rhot <- data.frame(Pic = seq_len(nrow(rv$files)), Color = color.choices[1]) %>%
        rhandsontable(selectCallback = TRUE, useTypes = FALSE, overflow = "visible",
                      highlightCol = TRUE, highlightRow = TRUE, rowHeaders=NULL, width=400) %>%
        hot_col(col="Pic", readOnly = TRUE, halign = "htCenter") %>% 
        hot_col(col="Color", readOnly = FALSE, type = "dropdown", source = color.choices,
                halign = "htLeft")
      rhot
    }) # end renderRHandsontable
    
    # Load images from file:
    rv$img.list <- lapply(rv$files$datapath, imager::load.image)
    
  })
  
  observeEvent(input$size,  {
    print("observeEvent(input$size)")
    rv$crop.size <- input$size
    print(rv$size)
  })
  
  observeEvent(input$coord.x,  {
    print("observeEvent(input$coord.x)")
    rv$crop.x <- input$coord.x
    print(rv$crop.x)
  })
  
  observeEvent(input$coord.y,  {
    print("observeEvent(input$coord.y)")
    rv$crop.y <- input$coord.y
    print(rv$crop.y)
  })
  
  observeEvent(input$gap, {
    print("observeEvent(input$gap)")
    rv$gap.size <- input$gap
    print(paste("New gap:", rv$gap.size))
  })
  
  observe({
    # Crop images ####
    print("observe() - 01")
    if (!is.null(rv$img.list)) {
      if (!any(is.na(c(rv$crop.size, rv$crop.x, rv$crop.y)))) {
        rv$img.list.crop <- lapply(rv$img.list, function(img) {
          my.crop(img, rv$crop.size, rv$crop.x, rv$crop.y)
        })
      } # end if
    } # end if
  }) # end observe
  
  observe({
    # Autocontrast ####
    print("observe() - 02 (Autocontrast)")
    if (!is.null(rv$img.list.crop)) {
      tmp <- lapply(rv$img.list.crop, my.rescale)
      rv$img.list.crop.rescale <- lapply(seq_along(rv$img.list.crop), function(i) {
        my.false.colorise(tmp[[i]], rv$color.list[i])
      }) # end lapply
    } # end if
  }) # end observe
  
  observe({
    # Composite cropped images ####
    print("observe() - 03 (Composite cropped images)")
    if (! is.null(rv$img.list.crop)) {
      img.gap <- make.gap(rv$img.list.crop, rv$gap.size, "white")
      # Make composite image
      rv$composite.original <- merge.pics(rv$img.list.crop, img.gap)
    } # end if
  }) # end observe
  
  observeEvent(input$hot, {
    # Add color ####
    print("observe() - 04 (Add color)")
    # Respond to changes in color choice table
    if (!is.null(input$hot)) {
      # Get row selected:
      hot.row <- input$hot_select$select$r
      if (!is.null(hot.row)) {
        print(paste("Row / image:", hot.row))
        color.selected <- input$hot$changes$changes[[1]][[4]]
        print(paste("Color chosen:", color.selected))
        rv$color.list[hot.row] <- color.selected
      } # end if
    } # end if
  }) # end observeEvent(input$hot)
  
  observe({
    # Composite of final images ####
    print("observe() - 05")
    if (! is.null(rv$img.list.crop.rescale)) {
      img.gap <- make.gap(rv$img.list.crop.rescale, rv$gap.size, "white")
      rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, img.gap)
    } # end if
  }) # end observe
  
  output$plot1 <- renderPlot({
    # Plot cropped images ####
    print("output$plot1")
    if (!is.null(rv$composite.original)) {
      plot(rv$composite.original, rescale=FALSE, main="Originals")
    } # end if
  }) # end renderPlot
  
  output$plot2 <- renderPlot({
    # Plot final images ####
    print("output$plot2 (Plot final images)")
    if (!is.null(rv$composite.rescaled)) {
      plot(rv$composite.rescaled, all=TRUE)
    } # end if
  }) # end renderPlot
  
  # Respond to Quit button
  observeEvent(input$navbar, {
    if (input$navbar == "stop") {
      print("Quitting app ...")
      stopApp()
    }
  })
})