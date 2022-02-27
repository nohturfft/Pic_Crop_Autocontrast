#------------------------------------------------------------------------------!
# Source code - functions ####
#------------------------------------------------------------------------------!

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
  print("--- Function: make.gap()")
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
  } else {
    res <- ff
  } # end if
  
  res
}



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
  
  print(paste("    -- run my.rescale...", farbe))
  bild.rescaled <- my.rescale(bild)
  
  if (farbe == "grayscale") {
    pic <- bild
  } else {
    pic <- EBImage::rgbImage(red=bild.rescaled * (my.rgb.colors[[farbe]][1] / 255),
                             green= bild.rescaled * (my.rgb.colors[[farbe]][2] / 255),
                             blue= bild.rescaled * (my.rgb.colors[[farbe]][3] / 255))
  }
  
  print("   -- done here. (my.false.colorise())")
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
  # panel.height <- txt.size + padding + padding
  panel.height <- (2* txt.size) + (2 * padding)
  panel.width <- imager::width(composite)
  txt <- paste0("Coord.x: ", x, "; Coord.y: ", y, " (", size, " X ", size, " px)", "\n",
                "Montage width: ", panel.width, " px")
  
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


squeeze_filename <- function(the.paths, max.char=24) {
  print("--- Function: squeeze_filename()")
  max.char.chunk <- ((max.char/2)-1)
  # max.char.chunk
  a <- basename(the.paths)
  b <- sapply(a, function(bn) {
    if (nchar(bn) > max.char) {
      chunk1 <- substr(bn, 1, max.char.chunk)
      chunk2 <- substr(bn, (nchar(bn)-max.char.chunk+1), nchar(bn))
      new.basename <- paste0(chunk1, "..", chunk2)
    } else {
      new.basename <- bn
    }
    new.basename
  })
  unname(b)
}
# squeeze_filename(x)
# debug(squeeze_filename)

my.session.info <- function() {
  if (isTRUE(rstudioapi::isAvailable())) {
    rstudio_version <- rstudioapi::versionInfo()$version
  } else {
    rstudio_version <- "?"
  }
  
  lib_paths <- c("\n\n.libPaths():", .libPaths(), "\n")
  
  rstudio_version <- paste("RStudio version:", rstudio_version)
  a <- capture.output(sessionInfo())
  b <- gsub("^ *\\[[0-9]*\\]", "", a)
  c <- stringr::str_trim(b)
  d <- paste(c(getwd(),
               rstudio_version, "\n",
               lib_paths,
               format(Sys.time(), "Date: %a %b %d, %Y. Time: %X"),
               paste("username:", system("whoami", intern=TRUE)),
               c))
  d
} # end function my.session.info()