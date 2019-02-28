# function to create a friction input layer from vector data
#' @title acc_friction
#'
#' @description Function to create a friction surface
#'
#' @param friction_input_1
#' @param friction_input_2
#' @param ...
#' @param my_filename
#' @param my_outputresolution
#' @param getproj
#' @param my_proj
#' @param mask
#' @param masklayer_path
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
                        my_filename,
                        my_outputresolution,
                        getproj = TRUE,
                        my_proj = NULL,
                        mask = FALSE,
                        masklayer_path = NULL) {
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
        filename = my_filename,
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
        srcfile = my_filename,
        dstfile = paste(my_filename, "_projected.tif", sep = ""),
        of = "GTiff",
        ot = "Byte",
        tr = c(my_outputresolution, my_outputresolution),
        te = paste(extent(tmp_friction)[c(1, 3, 2, 4)], collapse =
                     " "),
        cutline = masklayer_path,
        crop_to_cutline = mask,
        co = c("COMPRESS=LZW"),
        dstnodata = -9999
      )
      # create traveltimes per cell
      tmp_friction <-
        raster(paste(my_filename, "_projected.tif", sep = ""))
      tmp_cellsize <- my_outputresolution / 1000
      tmp_friction <-
        raster::calc(tmp_friction,
             function(x) {
               tmp_cellsize / (x / 3600)
             },
             filename = paste(my_filename, "_projected_traveltimes.tif", sep =
                                ""),
             datatype = "FLT4S",
             options = c("COMPRESS=LZW"))
      return(tmp_friction)
    }
  }
}
