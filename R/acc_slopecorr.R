# function to create a friction input layer from vector data
#' @title acc_slopecorr
#'
#' @description Function to correct friction base data for slope effects
#'
#' @param my_input
#' @param my_radians
#'
#' @return tmp_slopecorr
#'
#' @examples NULL
#'
#' @export acc_slopecorr
#'

# define function
acc_slopecorr <- function(my_input,
                          my_radians)
{
  # check for correct definition of input variables
  if (!inherits(my_input, c("RasterLayer"))) {
    stop('Please provide "my_input" as an object of Class RasterLayer.',
         call. = F)
  }
  if (!inherits(my_radians, c("RasterLayer"))) {
    stop('Please provide "my_radians" as an object of Class RasterLayer.',
         call. = F)
  }
  if (raster::res(my_input) != raster::res(my_radians) |
      raster::extent(my_input) != raster::extent(my_radians)|
      sp::proj4string(my_input) != sp::proj4string(my_radians)
      ) {
    stop('Extend and/or resolution and/or CRS of "input_data" and "my_radians" differ. Please homogenize before processing',
         call. = F)
  }
  # start processing
  tmp_stack <- raster::stack(my_input, my_radians)
  tmp_slopecorr <-
    tmp_stack[[1]] * (exp(-3 * tan(tmp_stack[[2]])))
  return(tmp_slopecorr)
}
