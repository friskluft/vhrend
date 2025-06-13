#' vthie_df_2_tree
#'
#' create a tree structure from a data.frame
#' (inspired by UROS-2018 conference)
#'
#' @param df a data.frame with specific format
#' @param scaleToPerc (logical) scale to percent
#'
#' @return a Node that can be written to json using \code{\link{vt_export_json}}

library (voronoiTreemap)

vthie_df_2_tree <- function (df, scaleToPerc=FALSE) {
  df$h1 <- as.character (df$h1)
  df$h2 <- as.character (df$h2)
  df$h3 <- as.character (df$h3)
  
  # checks
  stopifnot (is.data.frame(df))
  stopifnot (sum(is.na(df$weight))==0)
  stopifnot (is.numeric(df$weight))
  
  if (scaleToPerc) {
    df$weight <- df$weight/sum(df$weight)
  }
  
  n <- vt_create_node (df[1,1])
  spl <- split (df, factor(df$h2, levels=unique(df$h2)))
  
  if (any(sapply(spl, function(x) { length(!is.na(x$color)) })==0)) {
    stop("please specify at least one color for each second-level code (h2)")
  }
  
  for (i in seq_along(spl)) {
    tmp <- spl[[i]]
    # continent color
    ref <- as.character(tmp$h2[1])
    
    groupcol <- tmp$color[1]
    if (is.na(groupcol)) {
      groupcol <- tmp$color[!is.na(tmp$color)][1]
    }
    
    n <- vt_add_nodes (n, refnode=df[1,1], node_names=ref, colors=groupcol)
    
    for (j in 1:nrow(tmp)) {
      curnode <- tmp$h3[j]
      if (curnode==ref) {
        curnode <- paste(curnode, " ")
      }
      
      curcol <- tmp$color[j]
      if (is.na(curcol)) {
        curcol <- groupcol
      }
      curcode <- tmp$codes[j]
      if (is.na(curcode)) {
        curcode <- substr(tmp$h3[j], 1, 2)
      }
      n <- vt_add_nodes(
        n, 
        refnode=ref, 
        node_names=as.character(curnode),
        weights=tmp$weight[j], 
        codes=curcode, 
        colors=curcol)
    }
  }
  n
}

