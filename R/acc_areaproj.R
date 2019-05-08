# function to create a friction input layer from vector data
#' @title acc_areaproj
#'
#' @description Function to automatically retrive a good projection system for a given raster in proj4string
#'
#' @param my_input
#'
#' @return tmp_proj
#'
#' @examples NULL
#'
#' @export acc_areaproj

acc_areaproj <- function(my_input) {
  if (inherits(my_input, "Spatial"))
  {
    my_input <-
      spTransform(my_input,
                  CRS(projargs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  } else
  {
    my_input <-
      raster::projectRaster(my_input, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  }
  bb <- raster::extent(my_input)
  cntr_long <- (bb[3] - bb[1]) * 0.5 + bb[1]
  cntr_lat <- (bb[4] - bb[2]) * 0.5 + bb[2]
  bbm <- bb * 111000
  rng_x <- round(bbm[3] - bbm[1])
  rng_y <- round(bbm[4] - bbm[2])
  tmp_proj <- paste0(
    "+proj=laea +lat_0=",
    cntr_lat,
    " +lon_0=",
    cntr_long,
    " +x_0=",
    rng_x,
    " +y_0=",
    rng_y,
    " +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  )
  return(tmp_proj)
}
