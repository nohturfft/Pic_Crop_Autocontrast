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

options(shiny.maxRequestSize=10*1024^2)

#-------------------------------------------------------------------------------!
# Functions ####
#-------------------------------------------------------------------------------!
my.draw.rect2 <- function(img, x.top.left, y.top.left, width, stroke=5, color="green") {
  print("--- Function: my.draw.rect2()")
  # img[x.left:x.right, y.top:y.top+stroke-1] <- 1
  stopifnot(imager::spectrum(img) == 3)
  tmp <- img
  my.rgb <- c(1,2,3) %>% set_names(c("red", "green", "blue"))
  col.sel <- my.rgb[tolower(color)]
  # Horizontal line top:
  tmp[x.top.left:(x.top.left+width-1), y.top.left:(y.top.left+stroke-1), 1, col.sel] <- 1
  # Horizontal line bottom:
  tmp[x.top.left:(x.top.left+width-1), (y.top.left+width-stroke+1):(y.top.left+width), 1, col.sel] <- 1
  # Vertical line left:
  tmp[x.top.left:(x.top.left+stroke-1), y.top.left:(y.top.left+width-1), 1, col.sel] <- 1
  # Vertical line right:
  tmp[(x.top.left+width-stroke+1):(x.top.left+width-1), y.top.left:(y.top.left+width-1), 1, col.sel] <- 1
  tmp
}

my.rescale <- function(imge) {
  print("--- Function: my.rescale()")
  (imge - min(imge)) / (max(imge) - min(imge))
}

my.crop <- function(pic, breite, start.x, start.y) {
  print("--- Function: my.crop()")
  imager::imsub(pic,
                x %inr% c(start.x, start.x + breite - 1),
                y %inr% c(start.y, start.y + breite - 1))
}

make.gap <- function(image.list, gap.width, gap.col="white") {
  imager::imfill(x=gap.width, y=height(image.list[[1]]), z=1, val = gap.col) %>% 
    imager::grayscale(.)
}

intersperse.imgs <- function(pic1, pic2, pic.gap) {
  print("--- Function: intersperse.imgs()")
  list(pic1, pic.gap, pic2)
}

merge.pics <- function(pic.list, pic.gap) {
  print("--- Function: merge.pics()")
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

compose.pics <- function(pic.list, pic.gap, width.max) {
  print("--- Function: compose.pics()")
  if (!is.null(pic.gap)) {
    aa <- purrr::reduce(pic.list, intersperse.imgs, pic.gap=pic.gap)
    bb <- lapply(aa, function(y) {if (!is.list(y)) list(y) else y})
    cc <- purrr::flatten(bb)
  } else {
    cc <- pic.list
  }
  dd <- lapply(cc, EBImage::as.Image)
  ee <- lapply(dd, EBImage::toRGB)
  ff <- EBImage::abind(ee, along=1)
  
  if (imager::width(ff) > width.max) {
    res <- EBImage::resize(x=ff, w=width.max)
  } # end if
  
  res
}



# debug(compose.pics)

compose.info <- function(pic.list) {
  print("--- Function: compose.info()")
  # print("    -- dim(pic1):"); print(dim(pic1))
  # print("    -- dim(pic2):"); print(dim(pic2))
  # aa <- lapply(list(pic1, pic2), EBImage::as.Image)
  aa <- lapply(pic.list, EBImage::as.Image)
  print(sapply(aa, dim))
  # stopifnot(imager::width(pic1) == imager::width(pic2))
  bb <- lapply(aa, EBImage::toRGB)
  print(sapply(bb, dim))
  res <- EBImage::abind(bb, along=2)
  print("    -- done here. (compose.info)")
  res
}
# undebug(compose.info)

my.false.colorise <- function(bild, farbe) {
  print("--- Function: my.false.colorise()")
  print(paste("    -- farbe:", farbe))
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

make.scalebar.panel <- function(composite, bar.height, txt.size, txt, ofset, farbe, padding, breite.um, px.per.um) {
  print("--- Function: make.scalebar.panel()")
  
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
  
  print(paste("   -- panel.width:", panel.width))
  print(paste("   -- panel.height:", panel.height))
  print(paste("   -- bar.height:", bar.height))
  print(paste("   -- txt.size:", txt.size))
  print(paste("   -- inner.height:", inner.height))
  print(paste("   -- bar.width.px:", bar.width.px))
  print(paste("   -- descender:", descender))
  print(paste("   -- padding:", padding))
  print(paste("   -- padding.top:", padding.top))
  print(paste("   -- panel.height:", panel.height))
  print(paste("   -- text.y:", text.y))
  print(paste("   -- bar.y0:", bar.y0))
  print(paste("   -- bar.y1:", bar.y1))
  
  
  
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


make.file.info.panel <- function(basename.vector, composite, txt.size, padding) {
  print("--- Function: make.file.info.panel()")
  panel.height <- (txt.size * length(basename.vector)) + padding + padding
  panel.width <- imager::width(composite)
  padding.top <- padding
  pic <- imager::imfill(x=panel.width, y=panel.height, z=1, val = "white") %>% 
    imager::draw_text(., x=0, y=padding, text = paste(basename.vector, collapse="\n"),
                      color="gray40", fsize=txt.size) %>% 
    imager::grayscale(.)
  print("   -- done here. (make.file.info.panel)")
  pic
} # end fct make.file.info.panel()


make.selection.info.panel <- function(x, y, size, composite, txt.size, padding) {
  print("--- Function: make.selection.info.panel()")
  panel.height <- txt.size + padding + padding
  panel.width <- imager::width(composite)
  txt <- paste0("Coord.x: ", x, "; Coord.y: ", y, " (", size, " X ", size, " px)")
  
  pic <- imager::imfill(x=panel.width, y=panel.height, z=1, val = "white") %>% 
    imager::draw_text(., x=0, y=padding, text = txt, color="gray40", fsize=txt.size) %>% 
    imager::grayscale(.)
  print("   -- done here. (make.selection.info.panel)")
  pic
} # end fct make.selection.info.panel()

output.filename <- function(input.basename) {
  print("--- Function: output.filename()")
  sans.extn <- tools::file_path_sans_ext(input.basename)
  sans.extn.2 <- stringr::str_remove(sans.extn, "_[A-Za-z]+$")
  new.name <- paste0(sans.extn.2, "_Composite_X.png")
  new.name
} # end fct output.filename()

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
  text.height = 14,
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
    img.list = NULL,
    img.list.crop = NULL,
    img.list.correl = NULL,
    img.list.crop.correl = NULL,
    img.list.crop.rescale = NULL,
    img.list.mask = NULL,
    composite.original = NULL,
    composite.rescaled = NULL,
    composite.with.info = NULL,
    info.panel = NULL,
    crop.x = 1,
    crop.y = 1,
    crop.size = 500,
    gap.size = 15,
    overview = NULL,
    color.list = NULL,
    param_scalebar = parameters.scalebar,
    montage.max.width = 1500,
    correl.files = NULL
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
                        highlightCol = TRUE, highlightRow = TRUE, overflow = "auto") %>% 
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
      rv$correl.files <- basename(rv$files$name)[1:2]
      
      
      ## Update radio button under selection pic: ####
      updateRadioButtons(session=session, inputId = "radio_overview", choices = paste0("Pic", seq_along(rv$files$datapath)),
                         inline = TRUE, selected = "Pic2")
      
      ## Load images from file: ####
      # rv$img.list.original <- lapply(rv$files$datapath, imager::load.image) %>% 
      #   set_names(basename(rv$files$name))
      
      rv$img.list.original <- lapply(rv$files$datapath, function(fi) {
        a <- magick::image_read(fi)
        b <- imager::magick2cimg(a)
        c <- imager::grayscale(b)
        c
      }) %>% 
        
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
  
  
  # input$apply_max_width ####
  # observeEvent(input$apply_max_width, {
  #   print("observeEvent(input$apply_max_width)")
  #   # browser()
  #   print(class(rv$composite.rescaled))
  #   current.width <- imager::width(rv$composite.rescaled)
  #   if (imager::width(rv$composite.rescaled) >= input$montage_max_width) {
  #     rv$composite.rescaled <- EBImage::resize(x=EBImage::as.Image(isolate(rv$composite.with.info)),
  #                                              w=input$montage_max_width)
  #   } # end if
  #   print("... done here. (observeEvent(input$apply_max_width))")
  # }) # end observeEvent
  
  
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
      rv$correl.files <- c(fil1, fil2)
      rv$img.list.crop.correl <- rv$img.list.crop[c(fil1, fil2)]
      rv$img.list.correl <- rv$img.list[c(fil1, fil2)]
      print("... done here.")
    } # end if
  })
  
  # input$check_correl ####
  observe({
    print("observeEvent() - input$check_correl")
    # browser()
    if (input$check_correl == TRUE) {
      print(rv$correl.files)
      # show("div_plot_autocontrast")
      show("div_plot_correlation")
      ## Correlation table ####
      imgs.correl.complete <- rv$img.list[rv$correl.files]
      imgs.correl.selection <- rv$img.list.crop[rv$correl.files]
      if (input$radio_correl == "Selection") {
        imgs.correl.rescale <- lapply(imgs.correl.selection, my.rescale)
      } else {
        # Analyse entire image: 
        imgs.correl.rescale <- lapply(imgs.correl.complete, my.rescale)
      }
      print("... apply threshold mask:")
      # browser()
      if (input$check_mask == "TRUE") {
        # qtile.threshold <- paste0(input$mask_percent, "%")
        qtile.threshold <- as.numeric(input$mask_percent) / 100
        the.masks <- lapply(imgs.correl.rescale, function(im) {
          qtile <- quantile(im, qtile.threshold)
          # imager::threshold(img, thr=qtile, approx=FALSE)
          im >= qtile
        })
        mask.combined <- the.masks[[1]] | the.masks[[2]]
        rm(the.masks)
      } else {
        mask.combined <- array(rep(TRUE, imager::nPix(imgs.correl.rescale[[1]])),
                               dim(imgs.correl.rescale[[1]]))
      }
      
      imgs.correl <- lapply(imgs.correl.rescale, function(img) {
        img[!mask.combined] <- NA
        img
      })
      
      print("... prepare masked images for output:")
      rv$img.list.mask <- imager::imappend(list(as.cimg(mask.combined), imgs.correl[[1]]), axis = "x")
      
      print("... calculate correlations:")
      b <- imgs.correl %>%
        purrr::map(as.matrix) %>%
        purrr::map(as.vector) %>%
        set_names(c("x", "y"))
      
      names(b) <- c("x", "y")
      
      
      
      # Pearson correlation coefficient:
      pcc <- cor(b[[1]], b[[2]],
                    use = "pairwise.complete.obs")
      print(paste("pcc:", pcc))
      
      # Manders Overlap Coefficient
      moc <- sum(b[[1]] * b[[2]], na.rm=TRUE) / sqrt(sum(b[[1]]^2, na.rm=TRUE) * sum(b[[2]]^2, na.rm=TRUE))
      print(paste("moc:", moc))
      
      
      # Calculating linear model takes very long:
      # lm.results <- lm(y ~ x, data=b)
      # df.correl <- data.frame(Slope = lm.results$coefficients[2],
      #                         Intercept = lm.results$coefficients[1],
      #                         Correl = correl)
      
      df.correl <- data.frame(pcc = pcc,
                              moc = moc)
      
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
          if (input$check_overview_autocontrast == "TRUE") {
            print("Overview autocontrast: TRUE")
            img.tmp <- my.rescale(rv$img.list[[overview.indx]])
          } else {
            print("Overview autocontrast: FALSE")
            img.tmp <- rv$img.list[[overview.indx]]
          }
          
          frame.col <- ifelse(input$check_correl == "TRUE", "green", "red")
          
          # rv$overview <- img.tmp %>%
          overview.tmp <- img.tmp %>%
            imager::add.color() %>%
            my.draw.rect2(., x.top.left=isolate(rv$crop.x),
                          y.top.left=isolate(rv$crop.y),
                          width=isolate(rv$crop.size), stroke=10,
                          color=frame.col) %>%
            imager::resize(., size_x = new.width, size_y=new.height)
          
          # if (input$check_correl == "TRUE") {
          #   overview.tmp <- overview.tmp %>%
          #     my.draw.rect2(., x.top.left=isolate(rv$crop.x),
          #                   y.top.left=isolate(rv$crop.y),
          #                   width=isolate(rv$crop.size), stroke=10,
          #                   color="green")
          # }
          rv$overview <- overview.tmp
          
          show("plot_overview"); show("overview_options")
          # show("radio_overview"); show("check_overview_autocontrast")
          show("correlation_panel")
        } # end if
        
      } # end if
    } # end if
    print(paste("... done here (01).", Sys.time()))
  }) # end observe
  
  
  observe({
    # Autocontrast + pseudocolor ####
    # Trigger:
    # - rv$img.list.crop
    # - rv$color.list
    
    print("observe() 02: Autocontrast + color")
    if (!is.null(rv$img.list.crop)) {
      tmp <- lapply(rv$img.list.crop, my.rescale)
      rv$img.list.crop.rescale <- lapply(seq_along(rv$img.list.crop), function(i) {
        my.false.colorise(tmp[[i]], rv$color.list[i])
      }) # end lapply
    } # end if
    print(paste("... done here (Autocontrast + color).", Sys.time()))
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
    
    # Triggers:
    # - rv$img.list.crop.rescale
    # - rv$gap.size
    # - input$apply_max_width
    
    print("observe() 05: Assemble processed images")
    
    input$apply_max_width
    
    max.width <- isolate(input$montage_max_width)
    if (! is.null(rv$img.list.crop.rescale)) {
      if (rv$gap.size > 0) {
        img.gap <- make.gap(rv$img.list.crop.rescale, rv$gap.size, "white")
        rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, img.gap, max.width)
      } else {
        rv$composite.rescaled <- compose.pics(rv$img.list.crop.rescale, pic.gap=NULL, max.width)
      } # end if
    } # end if
    
    
    # if (imager::width(rv$composite.rescaled) > max.width) {
    #   rv$composite.rescaled <- EBImage::resize(x=rv$composite.rescaled, w=max.width)
    # } # end if
    
    print("... done here (05).")
  }) # end observe
  
  
  observe({
    # Final montage ####
    # browser()
    print("observe() - 06: Final montage")
    
    if (!is.null(rv$composite.rescaled)) {
        
        if (input$check_scalebar == "TRUE") {
          print("... scalebar TRUE")
          if (!any(sapply(rv$param_scalebar, is.na))) {
            ## Scalebar panel: ####
            scalebar.panel <- make.scalebar.panel(composite=rv$composite.rescaled,
                                          bar.height=rv$param_scalebar$bar.height,
                                          txt.size = rv$param_scalebar$text.height,
                                          breite.um=rv$param_scalebar$bar.width.um,
                                          px.per.um=rv$param_scalebar$px.per.um,
                                          txt="Text here",
                                          farbe=rv$param_scalebar$bar.color,
                                          padding=rv$param_scalebar$padding,
                                          ofset=rv$param_scalebar$bar.offset)
            print("... done making scalebar panel.")
            ## Input files panel: ####
            file.info.panel <- make.file.info.panel(basename(isolate(rv$files$name)),
                                                    composite=rv$composite.rescaled,
                                                    txt.size=rv$param_scalebar$text.height,
                                                    padding=rv$param_scalebar$padding)
            print("... done making file info panel.")
            ## Selection info panel: ####
            selection.info.panel <- make.selection.info.panel(x=isolate(rv$crop.x),
                                                              y=isolate(rv$crop.y),
                                                              size=isolate(rv$crop.size),
                                                              composite=rv$composite.rescaled,
                                                              txt.size=rv$param_scalebar$text.height,
                                                              padding=rv$param_scalebar$padding)
            
            all.info.panel <- compose.info(list(scalebar.panel, file.info.panel, selection.info.panel))
            composite.with.info <- compose.info(list(rv$composite.rescaled, all.info.panel))
          } else {
            composite.with.info <- isolate(rv$composite.rescaled)
          } # end if (!any(sapply(rv$param_scalebar, is.na)))
          
          if (imager::width(composite.with.info) >= rv$montage.max.width) {
            rv$composite.with.info <- EBImage::resize(x=composite.with.info, w=rv$montage.max.width)
          } else {
            rv$composite.with.info <- composite.with.info
          } # end if
        
      } else {
        # Remove scalebar
        print("... scalebar FALSE")
        rv$info.panel <- NULL
        rv$composite.with.info <- isolate(rv$composite.rescaled)
      } # end if (input$check_scalebar == "TRUE")
      
      show("div_plot_autocontrast")
      show("download_composite"); show("download_pics")
      
    } # end if (!is.null(rv$composite.rescaled))
    
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
    # Output plot: overview plot ####
    print("output$plot_overview")
    if (!is.null(rv$overview)) {
      par(mar=c(0,0,0,0))
      plot(rv$overview, axes = FALSE)
    } # end if
  }
  ) # end renderPlot
  
  output$plot_originals <- renderPlot({
    # Output plot: cropped images ####
    print("output$plot_originals")
    if (!is.null(rv$composite.original)) {
      par(mar=c(0,0,0,0))
      plot(rv$composite.original, rescale=FALSE, axes=FALSE)
    } # end if
  }
  ) # end renderPlot
  
  observe({
    output$plot_montage <- renderPlot({
      # Output plot: final montage ####
      print("output$plot_montage (Plot final images)")
      if (!is.null(rv$composite.rescaled)) {
        # plot(rv$composite.rescaled, all=TRUE)
        plot(rv$composite.with.info, all=TRUE)
      } # end if
    },
    height="auto"
    ) # end renderPlot
  })
  
  
  observe({
    # Output plot: masked selections ####
    if (!is.null(rv$img.list.mask)) {
      # browser()
      print("output$plot_correlation (Plot masks)")
      p <- ifelse(imager::width(rv$img.list.mask) <= rv$montage.max.width,
                  rv$img.list.mask,
                  EBImage::resize(x=rv$img.list.mask, w=rv$montage.max.width))
      print("... hello 1")
      output$plot_correlation <- renderPlot({
        plot(p, all=TRUE)
      })
      print("... hello 2")
    } # end if
  }) # end observe
  
  
  
  # Quit button ####
  observeEvent(input$navbar, {
    if (input$navbar == "stop") {
      print("Quitting app ...")
      stopApp()
    }
  })
})