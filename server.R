library(shiny)
library(imager)

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

shinyServer(function(input, output, server, session) {
  # browser()
  rv <- reactiveValues(
    files = NULL,
    img1 = NULL,
    img1.crop = NULL,
    img1.crop.rescale = NULL,
    img2 = NULL,
    img2.crop = NULL,
    img2.crop.rescale = NULL,
    composite.original = NULL,
    composite.rescaled = NULL,
    crop.x = 1,
    crop.y = 1,
    crop.size = 500,
    gap.size = 15,
    img.list = NULL,
    img.list.crop = NULL,
    img.list.crop.rescale = NULL
  )
  
  observeEvent(input$files, {
    print("observeEvent(input$files)")
    rv$files <- input$files
    # Load images from file:
    rv$img1 <- imager::load.image(rv$files$datapath[1])
    rv$img2 <- imager::load.image(rv$files$datapath[2])
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
    print("observe() - 01")
    if (!is.null(rv$img.list)) {
      rv$img.list.crop <- lapply(rv$img.list, function(img) {
        my.crop(img, rv$crop.size, rv$crop.x, rv$crop.y)
      })
    } # end if
  }) # end observe
  
  observe({
    print("observe() - 02")
    if (!is.null(rv$img.list.crop)) {
      rv$img.list.crop.rescale <- lapply(rv$img.list.crop, my.rescale)
    } # end if
  }) # end observe
  
  observe({
    print("observe() - 03")
    if (! is.null(rv$img.list.crop)) {
      img.gap <- make.gap(rv$img.list.crop, rv$gap.size, "white")
      # rv$composite.original <- imager::imappend(list(rv$img.list.crop[[1]], img.gap, rv$img.list.crop[[2]]), "x")
      rv$composite.original <- merge.pics(rv$img.list.crop, img.gap)
    } # end if
  }) # end observe
  
  observe({
    print("observe() - 04")
    if (! is.null(rv$img.list.crop.rescale)) {
      # browser()
      img.gap <- make.gap(rv$img.list.crop.rescale, rv$gap.size, "white")
      rv$composite.rescaled <- merge.pics(rv$img.list.crop.rescale, img.gap)
      dummy <- 1
    } # end if
  }) # end observe
  
  output$plot1 <- renderPlot({
    print("output$plot1")
    if (!is.null(rv$composite.original)) {
      plot(rv$composite.original, rescale=FALSE, main="Originals")
    } # end if
  }) # end renderPlot
  
  output$plot2 <- renderPlot({
    print("output$plot2")
    if (!is.null(rv$composite.rescaled)) {
      plot(rv$composite.rescaled, rescale=FALSE, main="Autocontrast")
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