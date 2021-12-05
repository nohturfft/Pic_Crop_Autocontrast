#-------------------------------------------------------------------------------!
# Graticule:
# Eclipse Ts2 - pixels per 100 micrometer:
# 4x: 68.7
# 20x: 342.4
# 40x: 689.5
#-------------------------------------------------------------------------------!
# Packages 
#-------------------------------------------------------------------------------!
library(BiocManager)
options(repos = BiocManager::repositories())
library(magick)
library(EBImage)
library(shiny)
library(shinyjs)
library(imager)
library(magrittr)
library(rhandsontable)
library(purrr)

# help(package="EBImage")
# help(package="imager")
# help(package="magick")

#-------------------------------------------------------------------------------!
# Functions ####
#-------------------------------------------------------------------------------!
my.draw.rect2 <- function(img, x.top.left, y.top.left, width, stroke=5) {
  print("... Function: my.draw.rect2()")
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
  print("... Function: my.rescale()")
  (imge - min(imge)) / (max(imge) - min(imge))
}

my.crop <- function(pic, breite, start.x, start.y) {
  print("... Function: my.crop()")
  imager::imsub(pic,
                x %inr% c(start.x, start.x + breite - 1),
                y %inr% c(start.y, start.y + breite - 1))
}

make.gap <- function(image.list, gap.width, gap.col="white") {
  imager::imfill(x=gap.width, y=height(image.list[[1]]), z=1, val = gap.col) %>% 
    imager::grayscale(.)
}

intersperse.imgs <- function(pic1, pic2, pic.gap) {
  print("... Function: intersperse.imgs()")
  list(pic1, pic.gap, pic2)
}

merge.pics <- function(pic.list, pic.gap) {
  print("... Function: merge.pics()")
  if (!is.null(pic.gap)) {
    aa <- purrr::reduce(pic.list, intersperse.imgs, pic.gap=pic.gap)
    bb <- lapply(aa, function(y) {if (!is.list(y)) list(y) else y})
    cc <- purrr::flatten(bb)
  } else {
    cc <- pic.list
  }
  stopifnot(class(cc)=="list")
  imager::imappend(cc, "x")
}
# debug(merge.pics)

compose.pics <- function(pic.list, pic.gap) {
  print("... Function: compose.pics()")
  if (!is.null(pic.gap)) {
    aa <- purrr::reduce(pic.list, intersperse.imgs, pic.gap=pic.gap)
    bb <- lapply(aa, function(y) {if (!is.list(y)) list(y) else y})
    cc <- purrr::flatten(bb)
  } else {
    cc <- pic.list
  }
  dd <- lapply(cc, EBImage::as.Image)
  ee <- lapply(dd, EBImage::toRGB)
  res <- EBImage::abind(ee, along=1)
  res
}
# debug(compose.pics)

compose.info <- function(pic1, pic2) {
  print("... Function: compose.info()")
  aa <- lapply(list(pic1, pic2), EBImage::as.Image)
  # stopifnot(imager::width(pic1) == imager::width(pic2))
  bb <- lapply(aa, EBImage::toRGB)
  res <- EBImage::abind(bb, along=2)
  res
}
# debug(compose.info)

my.false.colorise <- function(bild, farbe) {
  print("... Function: my.false.colorise()")
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

make.info.panel <- function(composite, bar.height, txt.size, txt, ofset, farbe, padding, breite.um, px.per.um) {
  print("... Function: make.info.panel()")
  
  # https://en.wikipedia.org/wiki/Typeface_anatomy
  # descender = pixels taken up by lower part of g j µ etc.
  descender <- floor(txt.size / 5)
  inner.height <- ifelse(txt.size > bar.height, txt.size, (bar.height + descender))
  panel.height <- inner.height + padding + padding
  
  panel.width <- imager::width(composite)
  bar.width.px <- floor(breite.um * px.per.um)
  
  padding.top <- padding
  
  if (txt.size == bar.height) {
    # Text and bar are the SAME height:
    text.y <- padding.top + descender
    bar.y0 <- padding + bar.height
  } else if (bar.height > txt.size) {
    # The bar is higher:
    text.y <- padding.top + descender + (bar.height - txt.size)
    bar.y0 <- padding + bar.height
  } else {
    # The text is higher:
    text.y <- padding.top #+ descender
    bar.y0 <- text.y + txt.size - descender
  }
  bar.y1 <- bar.y0 - bar.height
  
  print(paste("... panel.width:", panel.width))
  print(paste("... panel.height:", panel.height))
  print(paste("... bar.height:", bar.height))
  print(paste("... txt.size:", txt.size))
  print(paste("... inner.height:", inner.height))
  print(paste("... bar.width.px:", bar.width.px))
  print(paste("... descender:", descender))
  print(paste("... padding:", padding))
  print(paste("... padding.top:", padding.top))
  print(paste("... panel.height:", panel.height))
  print(paste("... text.y:", text.y))
  print(paste("... bar.y0:", bar.y0))
  print(paste("... bar.y1:", bar.y1))
  
  
  
  pic <- imager::imfill(x=panel.width, y=panel.height, z=1, val = "white") %>%
    # draw_rect: y0 measures from top, refers to BOTTOM edge of rectangle (y1 = top)
    imager::draw_rect(., x0 = ofset, x1 = (ofset + bar.width.px), y0 = bar.y0, y1 = bar.y1, color = farbe) %>% 
    # draw_txt: y measures from top, refers to TOP edge of text
    imager::draw_text(., x=(ofset + bar.width.px + bar.height),
                      y=text.y,
                      text=paste(breite.um, "um"), color="black", fsize=txt.size) %>% 
    imager::grayscale(.)
  pic
}
# debug(make.info.panel)

output.filename <- function(input.basename) {
  print("Function: output.filename()")
  sans.extn <- tools::file_path_sans_ext(input.basename)
  sans.extn.2 <- stringr::str_remove(sans.extn, "_[A-Za-z]+$")
  new.name <- paste0(sans.extn.2, "_Composite_X.png")
  new.name
}

#-------------------------------------------------------------------------------!
# Defaults ####
#-------------------------------------------------------------------------------!
color.choices <- c("Grayscale", "Green", "Blue", "Red")
mic.objectives <- c("4x", "10x", "20x", "40x", "Other")
pixels.per.micron <- c(0.687, 3.424, 6.895) %>% 
  magrittr::set_names(c("4x", "20x", "40x"))
scalebar.color.choices <- c("white", "black")

parameters.scalebar <- list(
  bar.height = 12,
  text.height = 26,
  bar.width.um = 20,
  objective = mic.objectives[3],
  px.per.um = 3.424,
  padding = 10,
  bar.color = scalebar.color.choices[2],
  bar.offset = 20
)

#-------------------------------------------------------------------------------!
# Server code ####
#-------------------------------------------------------------------------------!
shinyServer(function(input, output, server, session) {
  # Reactive values ####
  rv <- reactiveValues(
    files = NULL,
    composite.original = NULL,
    composite.rescaled = NULL,
    info.panel = NULL,
    composite.with.info = NULL,
    crop.x = 1,
    crop.y = 1,
    crop.size = 500,
    gap.size = 15,
    overview = NULL,
    img.list = NULL,
    img.list.crop = NULL,
    img.list.crop.correl = NULL,
    img.list.crop.rescale = NULL,
    color.list = NULL,
    param_scalebar = parameters.scalebar,
    img.list.crop.correl = NULL
  )
  
  # ui input defaults ####
  updateSelectInput(session=session, "scalebar_objective", choices = mic.objectives, selected=mic.objectives[3])
  updateNumericInput(session=session, "scalebar_px_per_um", value = parameters.scalebar$px.per.um)
  updateNumericInput(session=session, "scalebar_microns", value = parameters.scalebar$bar.width)
  updateNumericInput(session=session, "scalebar_height", value = parameters.scalebar$bar.height)
  updateNumericInput(session=session, "scalebar_txt_height", value = parameters.scalebar$text.height)
  updateNumericInput(session=session, "scalebar_padding", value = parameters.scalebar$padding)
  updateSelectInput(session=session, "scalebar_color", choices = scalebar.color.choices, selected=scalebar.color.choices[2])
  updateNumericInput(session=session, "scalebar_offset", value = parameters.scalebar$bar.offset)
  
  # input$files ####
  observeEvent(input$files, {
    # browser()
    print("observeEvent(input$files)")
    hide("download_composite"); hide("download_pics")
    hide("div_plot_originals")
    hide("div_plot_autocontrast")
    
    if (nrow(input$files) < 2) {
      show("file_count_error")
    } else {
      hide("file_count_error")
      
      rv$files <- input$files # data frame
      
      rv$color.list <- rep("grayscale", nrow(rv$files))
      
      ## rhandsontable: files & colors ####
      output$hot_files <- renderRHandsontable({
        fhot2 <- data.frame(Pic = seq_len(nrow(rv$files)), File = basename(rv$files$name), Color = color.choices[1]) %>% 
          rhandsontable(rowHeaders=NULL, width=400, useTypes = FALSE, stretchH = "all", selectCallback = TRUE,
                        highlightCol = TRUE, highlightRow = TRUE, overflow = "visible") %>% 
          hot_col(col="Pic", readOnly = TRUE, halign = "htCenter", format="text") %>% 
          hot_col(col="File", readOnly = FALSE, type = "dropdown", source = basename(rv$files$name)) %>% 
          hot_col(col="Color", readOnly = FALSE, type = "dropdown", source = color.choices)
        fhot2
      })
      
      ## rhandsontable: correl files ####
      output$hot_correl_files <- renderRHandsontable({
        cor.files <- basename(rv$files$name)[1:2]
        hot.correl <- data.frame(Axis = c("X Axis", "Y Axis"), File = names(rv$img.list.crop)[1:2]) %>% 
          rhandsontable(rowHeaders=NULL, width=400, useTypes = FALSE, stretchH = "all", selectCallback = TRUE,
                        highlightCol = TRUE, highlightRow = TRUE, overflow = "visible") %>% 
          hot_col(col="Axis", readOnly = TRUE, halign = "htLeft", format="text") %>%
          hot_col(col="File", readOnly = FALSE, type = "dropdown", format="text", source = names(rv$img.list.crop))
      })
      
      
      ## Update radio button under selection pic: ####
      updateRadioButtons(session=session, inputId = "radio_overview", choices = paste0("Pic", seq_along(rv$files$datapath)),
                         inline = TRUE, selected = "Pic2")
      
      ## Load images from file: ####
      rv$img.list.original <- lapply(rv$files$datapath, imager::load.image) %>% 
        set_names(basename(rv$files$name))
      
      rv$img.list <- rv$img.list.original %>% 
        set_names(names(rv$img.list.original))
    } # end if
    
  }) # end observeEvent(input$files)
  
  
  # input$size ####
  observeEvent(input$size,  {
    print("observeEvent(input$size)")
    # browser()
    rv$crop.size <- input$size
    print(paste("... rv$crop.size:", rv$crop.size))
  })
  
  # input$coord.x ####
  observeEvent(input$coord.x,  {
    print("observeEvent(input$coord.x)")
    rv$crop.x <- input$coord.x
    print(paste("... rv$crop.x:", rv$crop.x))
  })
  
  # input$coord.y ####
  observeEvent(input$coord.y,  {
    print("observeEvent(input$coord.y)")
    rv$crop.y <- input$coord.y
    print(paste("... rv$crop.y:", rv$crop.y))
  })
  
  # input$gap ####
  observeEvent(input$gap, {
    print("observeEvent(input$gap)")
    rv$gap.size <- input$gap
    print(paste("New gap:", rv$gap.size))
  })
  
  # input$check_gap ####
  observeEvent(input$check_gap, {
    if (input$check_gap == FALSE) {
      rv$gap.size <- 0
    } else {
      updateNumericInput(session=session, inputId = "gap", value=15)
      rv$gap.size <- 15
    }
  })
  
  # input$img_click ####
  observeEvent(input$img_click, {
    print("observeEvent(input$img_click)")
    # browser()
    click.x <- input$img_click$x
    click.y <- input$img_click$y
    x.max <- input$img_click$domain$right
    y.max <- input$img_click$domain$bottom
    img.width <- imager::width(isolate(rv$img.list[[1]]))
    img.height <- imager::height(isolate(rv$img.list[[1]]))
    crop.width <- isolate(rv$crop.size)
    crop.height <- isolate(rv$crop.size)
    
    top.left.x <- round(img.width * ( click.x / x.max), 0)
    if ((top.left.x + crop.width) > img.width) {
      top.left.x <- img.width - crop.width
    }
    print(paste("... top.left.x", top.left.x))
    
    top.left.y <- round(img.height * ( click.y / y.max), 0)
    if ((top.left.y + crop.height) > img.height) {
      top.left.y <- img.height - crop.height
    }
    print(paste("... top.left.y", top.left.y))
    
    updateNumericInput(session=session, "coord.x", value = top.left.x)
    updateNumericInput(session=session, "coord.y", value = top.left.y)
    print("... done here.")
  }) # end observeEvent(input$img_click)
  
  
  # input$hot_files ####
  observeEvent(input$hot_files, {
    print("observeEvent() - input$hot_files")
    if (!is.null(input$hot_files$changes$changes)) {
      
      # Get row selected:
      if (!is.null(input$hot_files_select$select)) {
        # browser()
        hot.row <- input$hot_files_select$select$r
        print(paste("... row selected:", hot.row))
        hot.col <- input$hot_files_select$select$c
        print(paste("... column selected:", hot.col))
        if (hot.col == 2) {
          file.selected <- input$hot_files$changes$changes[[1]][[4]]
          print(paste("... file selected:", file.selected))
          indx.img.selected <- which(basename(rv$files$name) == file.selected)
          print(paste("... indx.img.selected:", indx.img.selected))
          rv$img.list[[hot.row]] <- rv$img.list.original[[indx.img.selected]]
        } else if (hot.col == 3) {
          color.selected <- input$hot_files$changes$changes[[1]][[4]]
          print(paste("... Color chosen:", color.selected))
          rv$color.list[hot.row] <- color.selected
        } # end if
      } # end if
    } # end if
    print(paste("... done here (observeEvent(input$hot_files)).", Sys.time()))
  }) # end observeEvent(input$hot_files)
  
  # input$hot_correl_files ####
  observeEvent(input$hot_correl_files, {
    print("observeEvent() - input$hot_correl_files")
    # browser()
    if (!is.null(rv$img.list.crop)) {
      fil1 <- unlist(input$hot_correl_files$data[[1]][2])
      fil2 <- unlist(input$hot_correl_files$data[[2]][2])
      print(paste("... Correl file 1:", fil1))
      print(paste("... Correl file 2:", fil2))
      rv$img.list.crop.correl <- rv$img.list.crop[c(fil1, fil2)]
      print("... done here.")
    } # end if
  })
  
  # input$check_correl ####
  observe({
    print("observeEvent() - input$check_correl")
    # browser()
    if (input$check_correl == TRUE) {
      
      ## Correlation table ####
      a <- lapply(rv$img.list.crop.correl, my.rescale) %>%
        purrr::map(as.matrix) %>% 
        purrr::map(as.vector) %>% 
        set_names(c("x", "y"))
      lm.results <- lm(y ~ x, data=a)
      correl <- cor(a[[1]], a[[2]])
      df.correl <- data.frame(Slope = lm.results$coefficients[2],
                              Intercept = lm.results$coefficients[1],
                              Correl = correl)
      print(df.correl)
      output$table_correl <- renderTable(df.correl, digits=4)
      
    } else {
      print("Correlation checkbox FALSE")
    } # end if
  }) # end observeEvent(input$check_correl)
  
  
  
  
  
  # input$scalebar_objective ####
  observeEvent(input$scalebar_objective, {
    # Scalebar / objective
    print("observe() - 07 - Scalebar objective")
    # browser()
    if (input$scalebar_objective %in% names(pixels.per.micron)) {
      ppu <- pixels.per.micron[input$scalebar_objective] %>% unname
      updateNumericInput(session=session, inputId = "scalebar_px_per_um", value = ppu)
    } # end if
    print("... done here (07)")
  }) # end observeEvent(input$scalebar_objective)
  
  
  
  observe({
    print("observe() 01: Crop images + plot overview")
    input$selection_btn
    if (!is.null(rv$img.list)) {
      # browser()
      if (!any(is.na(c(isolate(rv$crop.size), rv$crop.x, rv$crop.y)))) {
        # Crop images ####
        print("... crop images ...")
        rv$img.list.crop <- lapply(rv$img.list, function(img) {
          my.crop(img, isolate(rv$crop.size), isolate(rv$crop.x), isolate(rv$crop.y))
        }) %>% 
          set_names(names(rv$img.list))
        rv$img.list.crop.correl <- rv$img.list.crop[1:2]
        
        # Plot overview ####
        print("... plot overview ...")
        new.width <- 400
        new.height <- round(((new.width / imager::width(rv$img.list[[1]])) * imager::height(rv$img.list[[1]])), 0)
        print(paste("... Overview image - width:", new.width))
        print(paste("... Overview image - height:", new.height))
        # browser()
        if (!is.null(input$radio_overview)) {
          print(paste("... input$radio_overview:", input$radio_overview))
          overview.indx <- input$radio_overview %>% stringr::str_remove("Pic") %>% as.numeric
          print(paste("... overview.indx:", overview.indx))
          rv$overview <- rv$img.list[[overview.indx]] %>%
            imager::add.color() %>%
            my.draw.rect2(., x.top.left=isolate(rv$crop.x),
                          y.top.left=isolate(rv$crop.y),
                          width=isolate(rv$crop.size), stroke=10) %>%
            imager::resize(., size_x = new.width, size_y=new.height)
          show("plot_overview"); show("radio_overview")
        } # end if
        
      } # end if
    } # end if
    print(paste("... done here (01).", Sys.time()))
  }) # end observe
  
  
  observe({
    # Autocontrast ####
    print("observe() 02: Autocontrast + color")
    if (!is.null(rv$img.list.crop)) {
      tmp <- lapply(rv$img.list.crop, my.rescale)
      rv$img.list.crop.rescale <- lapply(seq_along(rv$img.list.crop), function(i) {
        my.false.colorise(tmp[[i]], rv$color.list[i])
      }) # end lapply
    } # end if
    print(paste("... done here (02).", Sys.time()))
  }) # end observe
  
  
  observe({
    # Composite cropped images ####
    print("observe() 03: Composite cropped images - originals")
    if (! is.null(rv$img.list.crop)) {
      if (rv$gap.size > 0) {
        img.gap <- make.gap(rv$img.list.crop, rv$gap.size, "white")
        # Make composite image
        rv$composite.original <- merge.pics(rv$img.list.crop, img.gap)
      } else {
        rv$composite.original <- merge.pics(rv$img.list.crop, pic.gap=NULL)
      } # end if
      
      show("div_plot_originals")
    } # end if
    print(paste("... done here (03).", Sys.time()))
  }) # end observe
  
  
  observe({
    # Assemble processed images ####
    print("observe() 05: Assemble processed images")
    # browser()
    if (! is.null(rv$img.list.crop.rescale)) {
      if (rv$gap.size > 0) {
        img.gap <- make.gap(rv$img.list.crop.rescale, rv$gap.size, "white")
        rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, img.gap)
      } else {
        rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, pic.gap=NULL)
      } # end if
    } # end if
    print("... done here (05).")
  }) # end observe
  
  
  observe({
    print("observe() - 06: Final composite")
    
    if (!is.null(rv$composite.rescaled)) {
      if (input$check_scalebar == "TRUE") {
        if (!any(sapply(rv$param_scalebar, is.na))) {
          info.panel <- make.info.panel(composite=rv$composite.rescaled,
                                        bar.height=rv$param_scalebar$bar.height,
                                        txt.size = rv$param_scalebar$text.height,
                                        breite.um=rv$param_scalebar$bar.width.um,
                                        px.per.um=rv$param_scalebar$px.per.um,
                                        txt="Text here",
                                        farbe=rv$param_scalebar$bar.color,
                                        padding=rv$param_scalebar$padding,
                                        ofset=rv$param_scalebar$bar.offset)
          print("... done making info panel.")
          rv$composite.with.info <- compose.info(rv$composite.rescaled, info.panel)
        } else {
          rv$composite.with.info <- rv$composite.rescaled
        } # end if
        
      } else {
        # Remove scalebar
        print("... scalebar false")
        rv$info.panel <- NULL
        rv$composite.with.info <- rv$composite.rescaled
      } # end if
      
      show("div_plot_autocontrast")
      show("download_composite"); show("download_pics")
      
    } # end if
    
  }) # end observe
  
  
  observe({
    # Scalebar ####
    print("observe() 08: Scalebar")
    rv$param_scalebar <- list(
      bar.height = input$scalebar_height,
      text.height = input$scalebar_txt_height,
      bar.width.um = input$scalebar_microns,
      objective = input$scalebar_objective,
      px.per.um = input$scalebar_px_per_um,
      padding = input$scalebar_padding,
      bar.color = input$scalebar_color,
      bar.offset = input$scalebar_offset
    ) # end list
    
    if (input$check_scalebar == "TRUE") {
      print("... scalebar true")

    } else {
      # Remove scalebar
      print("... scalebar false")
      rv$info.panel <- NULL
    } # end if
    print("... done here (08)")
  }) # end observe
  
  
  
  
  
  # Download composite ####
  output$download_composite <- downloadHandler(
    filename = function () {
      output.filename(isolate(rv$files$name[1]))
    },
    content = function(file) {
      EBImage::writeImage(x=rv$composite.with.info, files=file, type="png")
    }
  ) # end downloadHandler
  
  # Download individual images ####
  observeEvent(input$download_pics, {
    print("observeEvent(input$download_pics)")
    if (!is.null(rv$img.list.crop.rescale)) {
      for (i in seq_along(rv$img.list.crop.rescale)) {
        pic.file <- paste0("Pic_", i, ".png")
        EBImage::writeImage(x=EBImage::as.Image(rv$img.list.crop.rescale[[i]]),
                            type="png",
                            files=pic.file)
        print(paste("... saved:", pic.file))
      } # end for
    } # end if
    print("Hello")
  }) # end observeEvent
  
  output$plot_overview <- renderPlot({
    # Output overview plot ####
    print("output$plot_overview")
    if (!is.null(rv$overview)) {
      par(mar=c(0,0,0,0))
      plot(rv$overview, axes = FALSE)
    } # end if
  }
  ) # end renderPlot
  
  output$plot_originals <- renderPlot({
    # Output plot cropped images ####
    print("output$plot_originals")
    if (!is.null(rv$composite.original)) {
      par(mar=c(0,0,0,0))
      plot(rv$composite.original, rescale=FALSE, axes=FALSE)
    } # end if
  }
  ) # end renderPlot
  
  observe({
    output$plot_montage <- renderPlot({
      # Output plot final images ####
      print("output$plot_montage (Plot final images)")
      if (!is.null(rv$composite.rescaled)) {
        # plot(rv$composite.rescaled, all=TRUE)
        plot(rv$composite.with.info, all=TRUE)
      } # end if
    },
    height="auto"
    ) # end renderPlot
  })
  
  
  
  # Quit button ####
  observeEvent(input$navbar, {
    if (input$navbar == "stop") {
      print("Quitting app ...")
      stopApp()
    }
  })
})