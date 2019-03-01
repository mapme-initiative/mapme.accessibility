# function to create a friction input layer from vector data
#' @title acc_slopecorr
#'
#' @description Function to correct friction base data for slope effects
#'
#' @param my_radians
#' @param my_input
#'
#' @return tmp_slopecorr
#'
#' @examples NULL
#'
#' @export acc_slopecorr
#'

# define function
acc_slopecorr <- function(my_input, my_radians)
{
  if (is.element("raster", installed.packages()[, 1]) == F) {
    print("You do not have 'raster' installed. Please install the package before proceeding")
  } else{
    tmp_stack <- stack(my_input, my_radians)
    tmp_slopecorr <- tmp_stack[[1]] * (exp(-3 * tan(tmp_stack[[2]])))
    return(tmp_slopecorr)
  }
}
