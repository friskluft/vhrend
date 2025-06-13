#' vthie_render
#'
#' create a tree structure from a data.frame
#'
#' @param datafname input CSV file path
#' @param outfname output HTML file path
#' @param width_ figure width in pixels
#' @param height_ figure height in pixels
#' @param color_border_ border color string e.g. "black"
#' @param border thickness in pixels, e.g. "2px"
#' @param figtit optional figure title
#' @param legetit optional legend title
#' 
#' @example source("vthie_df_2_tree.r"); source("vthie_df_2_tree.r"); vthie_render (datafname="/work/dataset1.csv", outfname="/work/figure1.html", width_=3000, height_=3000, color_border_="blue", border="3px", legetit="color legend:")
#'
#' @return none

vthie_render <- function (datafname, outfname, width_=1000, height_=1000, color_border_="black", border="1px", figtit="", legetit="") {
  
  # checks
  stopifnot(file.exists(datafname))
  
  # read data
  hiedata <- read.csv (datafname, fileEncoding="UTF-8-BOM")

  # create a tree
  T <- vthie_df_2_tree (hiedata, scaleToPerc=TRUE)
  
  # convert the tree to a json string
  J <- vt_export_json(T)
  
  # render
  showLegend <- ifelse (legetit=="", FALSE, TRUE)
  
  R <- vt_d3 (J, 
              label = TRUE,
              legend = showLegend,
              title = figtit, 
              legend_title = legetit, 
              size_border = border, 
              color_border = color_border_, 
              width=width_, 
              height=height_, 
              seed = 0)
  
  # save the graphics
  htmlwidgets::saveWidget (R, outfname)
}

