#------------------------------------------------------------------------------!
# Source code: default parameters ####
#------------------------------------------------------------------------------!

# Make sure large files can be opened:
options(shiny.maxRequestSize=20*1024^2)

# RGB colors for flse colorising grayscale images:
my.rgb.colors <- list(
  red=c(255, 0, 0),
  blue=c(115, 180, 255),
  green=c(50, 250, 0)
)

#-------------------------------------------------------------------------------!
# Defaults / ####
#-------------------------------------------------------------------------------!
color.choices <- c("Grayscale", "Green", "Blue", "Red")


#-------------------------------------------------------------------------------!
# Graticule:
# Eclipse Ts2 - pixels per 100 micrometer:
# 4x: 68.7
# 20x: 342.4
# 40x: 689.5
mic.objectives <- c("4x", "10x", "20x", "40x", "Other")

pixels.per.micron <- c(0.687, 3.424, 6.895) %>% 
  magrittr::set_names(c("4x", "20x", "40x"))

scalebar.color.choices <- c("white", "black")

default.parameters <- list(
  bar.height = 12,
  text.height = 30,
  bar.width.um = 20,
  objective = mic.objectives[3],
  px.per.um = 3.424,
  padding = 10,
  bar.color = scalebar.color.choices[2],
  bar.offset = 20,
  selection.px = 400,
  montage.max.width = 1800
)

