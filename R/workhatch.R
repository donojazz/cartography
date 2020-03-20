pat_circle <- function(x, cellsize) {
  x <- sf::st_union(x)
  centr <- sf::st_centroid(sf::st_geometry(x),
                           of_largest_polygon = FALSE)
  rad <- sqrt((diff(sf::st_bbox(x)[c(1, 3)]) ^ 2) +
                (diff(sf::st_bbox(x)[c(2, 4)]) ^ 2)) / 2
  ## alternative :
  rad <- max(sf::st_distance(centr, sf::st_cast(x, "POINT")))
  ntimes <- ceiling(rad / cellsize)
  
  # Initial circle #
  lp <- list()
  lp1 <- sf::st_buffer(centr, cellsize / 8)
  lp[[1]] <- sf::st_cast(lp1 , "LINESTRING")
  for (i in 1:ntimes) {
    join <- sf::st_buffer(centr, dist = (cellsize * i) + (cellsize / 8))
    lp[[i + 1]] <- sf::st_cast(join, "LINESTRING")
  }
  lp <- sf::st_sfc(do.call(rbind, lp), crs = sf::st_crs(x))
  endsf <- sf::st_intersection(lp, x)
  endsf <- endsf[sf::st_geometry_type(endsf)
                 %in% c("LINESTRING", "MULTILINESTRING")]
  endsf <- st_union(endsf)
  endsf
}



pat_dot <- function(x, cellsize){
  x <- st_union(x)
  fillgrid <- sf::st_make_grid(
    x = x, 
    cellsize = cellsize,
    what = "corners", square = FALSE
  )
  endsf <- fillgrid[sf::st_contains(x , fillgrid, sparse = FALSE)]
  endsf <- st_union(endsf)
  endsf
}


pat_grid <- function(x, cellsize, square){
  fillgrid <- sf::st_make_grid(
    x = x,
    cellsize = cellsize,
    what = "polygons",
    square = square
  )
  endsf <- sf::st_cast(fillgrid, "LINESTRING")
  endsf <- sf::st_intersection(endsf, x)
  endsf <- endsf[sf::st_geometry_type(endsf)
                 %in% c("LINESTRING", "MULTILINESTRING")]
  endsf <- sf::st_line_merge(sf::st_union(endsf))
}


pat_line <- function(x, cellsize, type){
  fillgrid <- sf::st_make_grid(
    x = x,
    cellsize = cellsize,
    what = "polygons",
    square = TRUE
  )
  ex <- list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  endsf <- lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[type]], 1:2]))
  endsf <- sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf <- sf::st_intersection(endsf, x)
  endsf <- endsf[sf::st_geometry_type(endsf)
                 %in% c("LINESTRING", "MULTILINESTRING")]
  endsf <- sf::st_line_merge(sf::st_union(endsf))
}


plot(mtq$geom, col = "gray90", lwd = .2)
x1 <- pat_circle(x = mtq[c(11,19, 15, 3), ], cellsize = 500)
plot(x1, add = T, col = 1)
x2 <- pat_dot(x = mtq[c(25, 1, 14, 18), ], cellsize = 2000)
plot(x2, add = T, col = 2, pch = 22, cex = .5)
x3 <- pat_grid(x = mtq[c(4, 5, 33, 34), ], cellsize = 2000, square = TRUE)
plot(x3, add = T, col = 3)
x4 <- pat_grid(x = mtq[c(8, 9, 29), ], cellsize = 3000, square = FALSE)
plot(x4, add = T, col = 4)
x5 <- pat_line(x = mtq[16, ], cellsize = 500, type = "horizontal")
plot(x5, add = T, col = 5)
x6 <- pat_line(x = mtq[28, ], cellsize = 500, type = "vertical")
plot(x6, add = T, col = 6)
x7 <- pat_line(x = mtq[12, ], cellsize = 1000, type = "left2right")
plot(x7, add = T, col = 7)
x8 <- pat_line(x = mtq[30, ], cellsize = 1500, type = "right2left")
plot(x8, add = T, col = 8)
# mapview::mapview(mtq)
# 
