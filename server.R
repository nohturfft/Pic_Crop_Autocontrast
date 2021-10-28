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
    gap.size = 15
  )
  
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
  
  observeEvent(input$files, {
    # browser()
    print("observeEvent(input$files)")
    rv$files <- input$files
    # Load images from file:
    rv$img1 <- imager::load.image(rv$files$datapath[1])
    rv$img2 <- imager::load.image(rv$files$datapath[2])
  })
  
  observeEvent(input$gap, {
    print("observeEvent(input$gap)")
    rv$gap.size <- input$gap
    print(paste("New gap:", rv$gap.size))
  })
  
  observe({
    # browser()
    print("observe() - 01")
    if (!is.null(rv$img1)) {
      rv$img1.crop <- my.crop(rv$img1, rv$crop.size, rv$crop.x, rv$crop.y)
    } # end if
  }) # end observe
  
  observe({
    print("observe() - 02")
    if (!is.null(rv$img2)) {
      rv$img2.crop <- my.crop(rv$img2, rv$crop.size, rv$crop.x, rv$crop.y)
    } # end if
  })
  
  observe({
    print("observe() - 03")
    if (!is.null(rv$img1.crop)) {
      rv$img1.crop.rescale <- my.rescale(rv$img1.crop)
    } # end if
  })
  
  observe({
    print("observe() - 04")
    if (!is.null(rv$img2.crop)) {
      rv$img2.crop.rescale <- my.rescale(rv$img2.crop)
    }
  })
  
  observe({
    print("observe() - 05")
    if (! any(c(is.null(rv$img1.crop), is.null(rv$img2.crop)))) {
      img.gap <- imager::imfill(x=rv$gap.size, y=height(rv$img1.crop), z=1, val = "yellow")
      rv$composite.original <- imager::imappend(list(rv$img1.crop, img.gap, rv$img2.crop), "x")
    } # end if
  })
  
  observe({
    print("observe() - 06")
    if (! any(c(is.null(rv$img1.crop.rescale), is.null(rv$img2.crop.rescale)))) {
      img.gap <- imager::imfill(x=rv$gap.size, y=height(rv$img1.crop.rescale), val = "white")
      rv$composite.rescaled <- imager::imappend(list(rv$img1.crop.rescale, img.gap, rv$img2.crop.rescale), "x")
    } # end if
  })
  
  
  output$plot1 <- renderPlot({
    print("output$plot1")
    if (!is.null(rv$composite.original)) {
      plot(rv$composite.original, rescale=FALSE)
    }
  })
  
  output$plot2 <- renderPlot({
    print("output$plot2")
    if (!is.null(rv$composite.rescaled)) {
      plot(rv$composite.rescaled, rescale=FALSE)
    }
  })
  
  # Respond to Quit button
  observeEvent(input$navbar, {
    if (input$navbar == "stop") {
      print("Quitting app ...")
      stopApp()
    }
  })
})