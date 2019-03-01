# function to calculate radians for friction correction
#' @title acc_radians
#'
#' @description Function to calculate radians from an elevation dataset
#'
#' @param my_input_dem
#' @param my_outputpath
#' @param my_baselayer
#' @param my_radians_alg
#'
#' @return tmp_radians
#'
#' @examples NULL
#'
#' @export acc_radians
#'

acc_radians <- function(my_input_dem,
                        my_outputpath,
                        my_baselayer,
                        my_radians_alg = "ZevenbergenThorne"){
  if (is.element("raster", installed.packages()[, 1]) == F) {
    print("You do not have 'raster' installed. Please install the package before proceeding")
  } else{
    if (is.element("gdalUtils", installed.packages()[, 1]) == F) {
      print(
        "You do not have 'galUtils' installed. Please install the package before proceeding"
      )
    } else{
      print("Start processing: (1) Homogenize slope layer with baselayer")
      gdalUtils::gdalwarp(
        srcfile = my_input_dem,
        dstfile = paste(my_outputpath, "dem_homogenized.tif", sep = ""),
        tr = res(my_baselayer),
        te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                     " "),
        r = "max",
        ot = "Byte",
        overwrite = F
      )

      gdalUtils::gdaldem(
        mode = "slope",
        input_dem = paste(my_outputpath, "dem_homogenized.tif", sep = ""),
        output =  paste(my_outputpath, "slope.tif", sep = ""),
        alg = my_radians_alg
      )
      print(
        paste(
          "Saved homogenized DEM and slope in:",
          my_outputpath,
          ".Can be removed afterwards. Starting now to calculate radians",
          sep = ""
        )
      )
      tmp_radians <-
        raster(paste(my_outputpath, "slope.tif", sep = "")) * (pi / 180)
      print("Finished processing")
      return(tmp_radians)

    }}}
