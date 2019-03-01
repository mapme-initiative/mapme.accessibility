# function to calculate radians for friction correction
#' @title acc_radians
#'
#' @description Function to calculate radians from an elevation dataset
#'
#' @param my_input_dem
#' @param my_outputpath
#' @param my_baselayer
#' @param my_radians_alg
#' @param save_results
#'
#' @return tmp_radians
#'
#' @examples NULL
#'
#' @export acc_radians
#'

acc_radians <- function(my_input_dem,
                        my_baselayer,
                        my_slope_alg = "ZevenbergenThorne",
                        my_outputpath = NULL,
                        save_results = FALSE) {
  if (is.element("raster", installed.packages()[, 1]) == F) {
    print("You do not have 'raster' installed. Please install the package before proceeding")
  } else{
    if (is.element("gdalUtils", installed.packages()[, 1]) == F) {
      print(
        "You do not have 'galUtils' installed. Please install the package before proceeding"
      )
    } else{
      print("Start Processing: Homogenize DEM layer with baselayer (gdal)")
      if (save_results == TRUE) {
        output_dir <- my_outputpath
      } else{
        output_dir <- tempdir()
      }
      gdalUtils::gdalwarp(
        srcfile = my_input_dem,
        dstfile = paste(output_dir, "/dem_homogenized.tif", sep = ""),
        tr = res(my_baselayer),
        te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                     " "),
        r = "max",
        ot = "UInt32",
        overwrite = F
      )
      print("Start processing: Create slope map (raster)")
      tmp_radians <-
        terrain(
          x = raster(paste(
            output_dir, "/dem_homogenized.tif", sep = ""
          )),
          opt = "slope",
          unit = "radians",
          filename = paste(output_dir, "/radians.tif", sep = "")
        )
    }
    print("Finished processing")
    return(tmp_radians)

  }
}
