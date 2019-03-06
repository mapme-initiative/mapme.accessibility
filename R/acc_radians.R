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
                        my_baselayer) {
  # check for correct definition of input variables
  if (!inherits(my_input, c("RasterLayer"))) {
    stop('Please provide "my_input" as an object of Class RasterLayer.',
         call. = F)
  }
  if (!inherits(my_radians, c("RasterLayer"))) {
    stop('Please provide "my_radians" as an object of Class RasterLayer.',
         call. = F)
  }
  # homogenized DEM layer if it differs from the baselayer
  if (raster::res(my_input) != raster::res(my_baselayer) |
      raster::extent(my_input) != raster::extent(my_baselayer)|
      sp::proj4string(my_input) != sp::proj4string(my_baselayer)) {
  print("Homogenize DEM layer with baselayer (gdal)")
  gdalUtils::gdalwarp(
    srcfile = my_input,
    dstfile = paste(tempdir(), "/dem_homogenized.tif", sep = ""),
    tr = res(my_baselayer),
    te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                 " "),
    s_srs = proj4string(my_input),
    t_srs = proj4string(my_baselayer),
    r = "mode",
    ot = "UInt32",
    overwrite = F
  )
  }else{raster::writeRaster(my_input,paste(tempdir(), "/dem_homogenized.tif", sep = ""))}
  print("Create slope map (raster)")
  tmp_radians <-
    terrain(
      x = raster(paste(
        tempdir(), "/dem_homogenized.tif", sep = ""
      )),
      opt = "slope",
      unit = "radians",
      filename = paste(tempdir(), "/radians.tif", sep = "")
    )
  print("Finished processing")
  return(tmp_radians)
  unlink(c(paste(tempdir, "/dem_homogenized.tif", sep =""),paste(tempdir, "/radians.tif", sep ="")))
}
