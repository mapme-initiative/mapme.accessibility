# function to create a friction input layer from vector data
#' @title acc_friction
#'
#' @description Function to create a friction surface
#'
#' @param friction_input_1
#' @param friction_input_2
#' @param ...
#' @param my_filename
#' @param my_filepath
#' @param my_outputresolution
#' @param getproj
#' @param my_proj
#'
#' @return tmp_friction
#'
#' @examples NULL
#'
#' @export acc_friction
#'


# define function
acc_frition <- function(friction_input_1,
                        friction_input_2,
                        ...,
                        my_filepath,
                        my_filename,
                        my_outputresolution,
                        getproj = TRUE,
                        my_proj = NULL) {
  if (is.element("raster", installed.packages()[, 1]) == F) {
    print("You do not have 'raster' installed. Please install the package before proceeding")
  } else{
    if (is.element("gdalUtils", installed.packages()[, 1]) == F) {
      print(
        "You do not have 'galUtils' installed. Please install the package before proceeding"
      )
    } else{
      library(raster)
      library(gdalUtils)
      # stack the layers
      tmp_stack <- stack(friction_input_1, friction_input_2, ...)
      # take max
      tmp_friction <- stackApply(
        tmp_stack,
        indices = rep(1, length(tmp_stack@layers)),
        # one element in "indices" for each input layer
        fun = max,
        filename = paste(my_filepath,my_filename,".tif",sep=""),
        datatype = "INT1U",
        options = c("COMPRESS=LZW")
      )
      # project the raster
      if (getproj == TRUE) {
        tmp_proj <- acc_areaproj(tmp_friction)
      } else{
        tmp_proj <- my_proj
      }
      gdalUtils::gdalwarp(
        srcfile = paste(my_filepath,my_filename,".tif",sep=""),
        dstfile = paste(my_filepath,my_filename, "_projected.tif", sep = ""),
        of = "GTiff",
        ot = "Byte",
        tr = c(my_outputresolution, my_outputresolution),
        co = c("COMPRESS=LZW"),
        s_srs = proj4string(tmp_friction),
        t_srs = tmp_proj,
        dstnodata = -9999
      )
      # create traveltimes in seconds to cross one cell
      tmp_friction <-
        raster(paste(my_filepath,my_filename, "_projected.tif", sep = ""))
      tmp_friction <-
        raster::calc(tmp_friction,
             function(x) {
               my_outputresolution/((x*1000)/3600)
             },
             filename = paste(my_filepath,my_filename, "_projected_traveltimes.tif", sep =
                                ""),
             datatype = "INT4U",
             options = c("COMPRESS=LZW"))
      return(tmp_friction)
    }
  }
}
