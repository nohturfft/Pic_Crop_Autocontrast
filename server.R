#-------------------------------------------------------------------------------!
# Packages 
#-------------------------------------------------------------------------------!
library(BiocManager)
options(repos = BiocManager::repositories())
library(magick)
library(EBImage)
library(shiny)
library(imager)
library(magrittr)
library(rhandsontable)
library(purrr)

# help(package="EBImage")
# help(package="imager")
# help(package="magick")

#-------------------------------------------------------------------------------!
# Functions
#-------------------------------------------------------------------------------!
my.draw.rect2 <- function(img, x.top.left, y.top.left, width, stroke=5) {
  # img[x.left:x.right, y.top:y.top+stroke-1] <- 1
  stopifnot(imager::spectrum(img) == 3)
  tmp <- img
  # Horizontal line top:
  tmp[x.top.left:(x.top.left+width-1), y.top.left:(y.top.left+stroke-1), 1, 1] <- 1
  # Horizontal line bottom:
  tmp[x.top.left:(x.top.left+width-1), (y.top.left+width-stroke+1):(y.top.left+width), 1, 1] <- 1
  # Vertical line left:
  tmp[x.top.left:(x.top.left+stroke-1), y.top.left:(y.top.left+width-1), 1, 1] <- 1
  # Vertical line right:
  tmp[(x.top.left+width-stroke+1):(x.top.left+width-1), y.top.left:(y.top.left+width-1), 1, 1] <- 1
  tmp
}

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
    overview = NULL,
    img.list = NULL,
    img.list.crop = NULL,
    img.list.crop.rescale = NULL,
    color.list = NULL
    # tmp = NULL
  )
  
  # input$files
  observeEvent(input$files, {
    # browser()
    print("observeEvent(input$files)")
    rv$files <- input$files # data frame
    rv$color.list <- rep("grayscale", nrow(rv$files))
    
    output$hot_files <- renderRHandsontable({
      fhot <- data.frame(Pic = seq_len(nrow(rv$files)), File = basename(rv$files$name)) %>% 
        rhandsontable(overflow = "visible", rowHeaders=NULL, width=400) %>% 
        hot_col(col="Pic", readOnly = TRUE, halign = "htCenter") %>% 
        hot_col(col="File", readOnly = TRUE, halign = "htLeft")
      fhot
    })
    
    output$hot_colors <- renderRHandsontable({
      rhot <- data.frame(Pic = seq_len(nrow(rv$files)), Color = color.choices[1]) %>%
        rhandsontable(selectCallback = TRUE, useTypes = FALSE, overflow = "visible",
                      highlightCol = TRUE, highlightRow = TRUE, rowHeaders=NULL, width=400) %>%
        hot_col(col="Pic", readOnly = TRUE, halign = "htCenter") %>% 
        hot_col(col="Color", readOnly = FALSE, type = "dropdown", source = color.choices,
                halign = "htLeft")
      rhot
    }) # end renderRHandsontable
    
    # Load images from file: ####
    rv$img.list <- lapply(rv$files$datapath, imager::load.image)
    
  }) # end observeEvent(input$files)
  
  # input$size ####
  observeEvent(input$size,  {
    print("observeEvent(input$size)")
    rv$crop.size <- input$size
    print(rv$size)
  })
  
  # input$coord.x ####
  observeEvent(input$coord.x,  {
    print("observeEvent(input$coord.x)")
    rv$crop.x <- input$coord.x
    print(rv$crop.x)
  })
  
  # input$coord.y ####
  observeEvent(input$coord.y,  {
    print("observeEvent(input$coord.y)")
    rv$crop.y <- input$coord.y
    print(rv$crop.y)
  })
  
  # input$gap ####
  observeEvent(input$gap, {
    print("observeEvent(input$gap)")
    rv$gap.size <- input$gap
    print(paste("New gap:", rv$gap.size))
  })
  
  # input$img_click ####
  observeEvent(input$img_click, {
    print("observeEvent(input$img_click)")
    # browser()
    click.x <- input$img_click$x
    click.y <- input$img_click$y
    x.max <- input$img_click$domain$right
    y.max <- input$img_click$domain$bottom
    top.left.x <- round(imager::width(isolate(rv$img.list[[1]])) * ( click.x / x.max), 0)
    print(paste("top.left.x", top.left.x))
    top.left.y <- round(imager::height(isolate(rv$img.list[[1]])) * ( click.y / y.max), 0)
    print(paste("top.left.y", top.left.y))
    
    updateNumericInput(session=session, "coord.x", value = top.left.x)
    updateNumericInput(session=session, "coord.y", value = top.left.y)
    print("   ... done here.")
  })
  
  observe({
    print("observe() - 01")
    if (!is.null(rv$img.list)) {
      if (!any(is.na(c(rv$crop.size, rv$crop.x, rv$crop.y)))) {
        
        # Crop images ####
        rv$img.list.crop <- lapply(rv$img.list, function(img) {
          my.crop(img, rv$crop.size, rv$crop.x, rv$crop.y)
        })
        
        # Plot overview ####
        new.width <- 400
        new.height <- round(((new.width / imager::width(rv$img.list[[1]])) * imager::height(rv$img.list[[1]])), 0)
        # print("Overview image - width:", new.width)
        # print("Overview image - height:", new.height)
        rv$overview <- rv$img.list[[1]] %>%
          imager::add.color() %>%
          my.draw.rect2(., x.top.left=rv$crop.x,
                        y.top.left=rv$crop.y,
                        width=rv$crop.size, stroke=10) %>%
          imager::resize(., size_x = new.width, size_y=new.height)
        
      } # end if
    } # end if
  }) # end observe
  
  observe({
    # Autocontrast ####
    print("observe() - 02 (Autocontrast + color)")
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
  
  observeEvent(input$hot_colors, {
    # Add color ####
    print("observe() - 04 (Add color)")
    # Respond to changes in color choice table
    if (!is.null(input$hot_colors$changes$changes)) {
      # Get row selected:
      hot.row <- input$hot_colors_select$select$r
      if (!is.null(hot.row)) {
        print(paste("Row / image:", hot.row))
        color.selected <- input$hot_colors$changes$changes[[1]][[4]]
        print(paste("Color chosen:", color.selected))
        rv$color.list[hot.row] <- color.selected
      } # end if
    } # end if
  }) # end observeEvent(input$hot_colors)
  
  observe({
    # Composite of final images ####
    print("observe() - 05")
    if (! is.null(rv$img.list.crop.rescale)) {
      img.gap <- make.gap(rv$img.list.crop.rescale, rv$gap.size, "white")
      rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, img.gap)
    } # end if
    print("... done here.")
  }) # end observe
  
  output$download <- downloadHandler(
    filename = function () {
      a <- "composite.png"
      a
    },
    content = function(file) {
      EBImage::writeImage(x=rv$composite.rescaled, files=file, type="png")
    }
  )
  
  output$plot_overview <- renderPlot({
    # Output overview plot ####
    print("output$plot_overview")
    if (!is.null(rv$overview)) {
      par(mar=c(0,0,0,0))
      plot(rv$overview, axes = FALSE, frame.plot=FALSE)
    } # end if
  }
  ) # end renderPlot
  
  output$plot1 <- renderPlot({
    # Output plot cropped images ####
    print("output$plot1")
    if (!is.null(rv$composite.original)) {
      par(mar=c(0,0,0,0))
      plot(rv$composite.original, rescale=FALSE, main="Originals", axes=FALSE)
    } # end if
  }
  ) # end renderPlot
  
  output$plot2 <- renderPlot({
    # Output plot final images ####
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