#' @title Typology Layer
#' @name typoLayer
#' @description Plot a typology layer.
#' @param x an sf object, a simple feature collection. If x is used then spdf, df, spdfid and dfid are not.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid name of the identifier variable in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid name of the identifier variable in df, default to the first column 
#' of df. (optional)
#' @param var name of the variable to plot.
#' @param col a vector of colors.
#' @param border color of the polygons borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.order values order in the legend, a character vector 
#' that matches var modalities. Colors will be affected following this order.  
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata no data label.
#' @param colNA no data color. 
#' @param pchNA no data pch
#' @param cex cex
#' @param pch pch
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @seealso \link{propSymbolsTypoLayer}, \link{typoLayer}, \link{legendTypo}
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' typoLayer(x = mtq, var="STATUS",  
#'           col = c("aquamarine4", "yellow3","wheat"),
#'           legend.values.order = c("Prefecture",
#'                                   "Sub-prefecture", 
#'                                   "Simple municipality"),
#'           legend.pos = "topright",
#'           legend.title.txt = "Status")
#' layoutLayer(title = "Municipality Status")
typoLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, var, 
                      col = NULL, border = "grey20", lwd = 1,
                      colNA = "white",
                      legend.pos = "bottomleft", 
                      legend.title.txt = var,
                      legend.title.cex = 0.8, 
                      legend.values.cex = 0.6,
                      legend.values.order = NULL,
                      legend.nodata = "no data",
                      legend.frame = FALSE, pch = 20,cex = 2, 
                      pchNA = 8,
                      add = FALSE)
{
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  
  # modalities
  mod <- unique(x[[var]])
  mod <- mod[!is.na(mod)]
  
  # check nb col vs nb mod
  col <- checkCol(col, mod)
  pch <- checkPch(pch, mod)
  
  # check legend.values.order vs mod values
  legend.values.order <- checkOrder(legend.values.order, mod)
  
  
  # get the colors 
  refcol <- data.frame(mod = legend.values.order, 
                       col = col[1:length(legend.values.order)],
                       pch = pch[1:length(legend.values.order)],
                       stringsAsFactors = FALSE)

  colVec <- refcol[match(x[[var]], refcol[,1]),2]
  pchVec <- refcol[match(x[[var]], refcol[,1]),3]
  
  
  # for NA values
  nodata <- FALSE
  if(max(is.na(x[[var]]) > 0)){
    nodata <- TRUE
    colVec[is.na(colVec)] <- colNA
    pchVec[is.na(pchVec)] <- pchNA
  }
  
  # plot
  if (is(sf::st_geometry(x), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))){
    cx <- 'line'
  }
  if (is(sf::st_geometry(x), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
    cx <- 'poly'
  }
  if (is(sf::st_geometry(x), c("sfc_POINT", "sfc_MULTIPOINT"))){
    cx <- 'point'
  }
  
  
  
  switch(
    cx, 
    line = {
      plot(sf::st_geometry(x), col = colVec, lwd = lwd, add = add)
      symbol <- "line"
    }, 
    poly = {plot(sf::st_geometry(x), col = colVec, border = border, 
                 lwd = lwd, add = add)
      legend.border <- border
      symbol <- "box"
    }, 
    point = {
      bg <- rep(NA, length(pchVec))
      bg[pchVec %in% 21:25] <- colVec[pchVec %in% 21:25]
      colVec[pchVec %in% 21:25] <- border
      plot(sf::st_geometry(x), col = colVec, pch = pchVec, cex = cex[1], 
           lwd = lwd, add = add, bg = bg)
      symbol <- "point"
      legend.border <- border
    }
  )
  
  
  
  
  
  
  legendTypo(pos = legend.pos, title.txt = legend.title.txt,
             title.cex = legend.title.cex, values.cex = legend.values.cex,
             categ = refcol[,1], 
             col = refcol[,2], 
             pch = refcol[,3],
             pt.cex = cex[1],
             border = legend.border,
             frame = legend.frame, 
             symbol = symbol, 
             nodata = nodata,
             nodata.col = colNA,
             nodata.pch = pchNA,
             nodata.txt = legend.nodata)
  
}
