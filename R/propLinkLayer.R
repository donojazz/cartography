#' @name propLinkLayer
#' @title Proportional Links Layer
#' @description Plot a layer of proportional links. Links widths are directly proportional to values of a variable.
#' @param x an sf object, a simple feature collection.
#' @param df a data frame that contains identifiers of starting and ending points and a variable.
#' @param xid names of the identifier variables in x, character 
#' vector of length 2, default to the 2 first columns. (optional)
#' @param dfid names of the identifier variables in df, character 
#' vector of length 2, default to the two first columns. (optional)
#' @param var name of the variable used to plot the links widths.
#' @param maxlwd maximum size of the links.
#' @param col color of the links.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values 
#' displayed in the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param spdf defunct.
#' @param spdfid defunct.
#' @param spdfids defunct.
#' @param spdfide defunct.
#' @param dfids defunct.
#' @param dfide defunct.
#' @note Unlike most of cartography functions, identifiers variables are mandatory.
#' @seealso \link{gradLinkLayer}, \link{getLinkLayer}, \link{legendPropLines}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
#' # Create a link layer - work mobilities to Fort-de-France (97209)
#' mob.sf <- getLinkLayer(x = mtq, df = mob[mob$j==97209,], dfid = c("i", "j"))
#' # Plot the links - Work mobility
#' plot(st_geometry(mtq), col = "grey60",border = "grey20")
#' propLinkLayer(x = mob.sf, df = mob,
#'               maxlwd = 10,
#'               legend.pos = "topright",
#'               var = "fij",
#'               col = "#92000090", add = TRUE)
#' @export
propLinkLayer <- function(x, df, xid = NULL, dfid = NULL, 
                          var, maxlwd = 40, 
                          col, 
                          legend.pos = "bottomleft",  
                          legend.title.txt = var, 
                          legend.title.cex = 0.8, 
                          legend.values.cex = 0.6, 
                          legend.values.rnd = 0,
                          legend.frame = FALSE, 
                          add = TRUE,
                          spdf, spdfid, spdfids, spdfide, dfids, dfide){
  
  if(sum(c(missing(spdf), missing(spdfid), missing(spdfids), 
           missing(spdfide), missing(dfids), missing(dfide))) != 6){
    stop("spdf, spdfid, spdfids, spdfide, dfids and dfide are defunct arguments; last used in version 1.4.2.",
         call. = FALSE)
  }
  if (is.null(xid)){xid <- names(x)[1:2]}
  if (is.null(dfid)){dfid <- names(df)[1:2]}
  
  # joint
  link <- merge(x = x[,xid], y = df, by.x = xid, by.y = dfid)
  # clean
  link <- link[!is.na(link[[var]]), ]
  
  maxval <- max(link[[var]])
  link$lwd <- link[[var]] * maxlwd / maxval
  plot(sf::st_geometry(link), lwd = link$lwd, col = col, add = add)
  
  legendPropLines(pos = legend.pos, title.txt = legend.title.txt, 
                  title.cex = legend.title.cex,
                  values.cex = legend.values.cex, 
                  var = c(min(link[[var]]), max(link[[var]])), 
                  lwd = maxlwd, col = col, frame = legend.frame, 
                  values.rnd = legend.values.rnd)
}
