# function to calculate radians for friction correction
#' @title acc_radians
#'
#' @description Function to calculate radians from an elevation dataset
#'
#' @param my_input
#' @param my_baselayer
#'
#' @return tmp_radians
#'
#' @examples NULL
#'
#' @export acc_radians
#'

acc_radians <- function(my_input,
                        my_baselayer,
                        resampling_method="average") {
  # check for correct definition of input variables
  if (!inherits(my_input, c("RasterLayer"))) {
    stop('Please provide "my_input" as an object of Class RasterLayer.',
         call. = F)
  }
  if (!inherits(my_baselayer, c("RasterLayer"))) {
    stop('Please provide "my_baselayer" as an object of Class RasterLayer.',
         call. = F)
  }
  # create slop map
  print("Create slope map (raster)")
  filename_1<-tempfile(pattern = "raster_",fileext = ".tif")
  tmp_radians <-
    terrain(
      x = my_input,
      opt = "slope",
      unit = "radians",
      filename = filename_1
    )
  tmp_radians<-raster(filename_1)
  # homogenized DEM layer if it differs from the baselayer
  if (raster::res(my_input) != raster::res(my_baselayer) ||
      raster::extent(my_input) != raster::extent(my_baselayer)||
      sp::proj4string(my_input) != sp::proj4string(my_baselayer)) {
    print("Homogenize DEM layer with baselayer (gdal)")
    filename_2<-tempfile(pattern = "raster_",fileext = ".tif")
    gdalUtils::gdalwarp(
      srcfile = filename_1,
      dstfile = filename_2,
      tr = res(my_baselayer),
      te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                   " "),
      s_srs = proj4string(my_input),
      t_srs = proj4string(my_baselayer),
      r = resampling_method,
      ot = "UInt32",
      overwrite = F
    )
    tmp_radians<-raster(filename_2)
  }
  print("Finished processing")
  return(tmp_radians)
}
