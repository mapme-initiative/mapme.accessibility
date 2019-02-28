# function to create a friction input layer from vector data
#' @title acc_accessibility
#'
#' @description Function to calculate accessiblity map by
#' # create a mapregion from friction raster
#' # Create a new GrassDB, set location and mapset
#' # Import friction raster to GRASS
#' # set  grass region from raster
#' # Import soures - destinites making sure both have same CRS
#' # calculating accumulated costmap
#' # exporting cost map as raster
#'
#'
#' @param my_input
#' @param my_outputpath
#' @param my_outputname
#' @param my_baselayer
#' @param my_reclassmatrix
#' @param my_speedfield
#' @param my_outputpath
#'
#' @return r_tmp
#'
#' @examples NULL
#'
#' @export acc_accessibility
#'

# define function
