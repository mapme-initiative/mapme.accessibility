# function to create a friction input layer from vector data
#' @title acc_ras2fric
#'
#' @description Function to convert vector to friction base data
#'
#' @param my_input
#' @param my_baselayer
#' @param resampling_method
#' @param my_reclass_inputvalues
#' @param my_reclass_outputvalues
#' @param my_datatype
#'
#' @return tmp_raster
#'
#' @examples NULL
#'
#' @export acc_ras2fric
#'

# define function
acc_ras2fric <-
  function(my_input,
           my_baselayer,
           resampling_method="max",
           my_reclass_inputvalues = NULL,
           my_reclass_outputvalues = NULL,
           my_datatype = "UInt16")
  {
    # check for correct definition of input variables
    if (!inherits(my_baselayer, c("RasterLayer"))) {
      stop('Please provide "my_baselayer" as an object of Class RasterLayer.',
           call. = F)
    }
    if (!inherits(my_input, c("RasterLayer"))) {
      stop('Please provide "my_input" as an object of Class RasterLayer',
           call. = F)
    }
    if (!is.null(my_reclass_inputvalues) &&
        is.null(my_reclass_outputvalues)) {
      stop(
        'You provided reclassification input values without providing output values. Please specify a vector of the same length with output values.',
        call. = F
      )
    }
    if (is.null(my_reclass_inputvalues) &&
        !is.null(my_reclass_outputvalues)) {
      stop(
        'You provided reclassification output values without providing input values. Please specify a vector of the same length with input values.',
        call. = F
      )
    }
    if (!is.null(my_reclass_inputvalues) &&
        !is.null(my_reclass_outputvalues) &&
        length(my_reclass_inputvalues) != length(my_reclass_outputvalues)) {
      stop(
        'Input and Outputvalues do not share the same length. Please provide two vectors with equal length to reclassify',
        call. = F
      )
    }
    # check  wheter the data should be reclassified
    if (!is.null(my_reclass_inputvalues)) {
      print("You provided a reclassification matrix. Starting to reclassify the raster")
      # reclassify
      filename_1<-tempfile(pattern = "raster_",fileext = ".tif")
      raster::reclassify(
        my_input,
        cbind(my_reclass_inputvalues, my_reclass_outputvalues),
        include.lowest = T,
        filename = filename_1,
        datatype = "INT2U"
      )
      # rescale the raster if resolution and or extent differs
      if (raster::res(my_input) != raster::res(my_baselayer) ||
          raster::extent(my_input) != raster::extent(my_baselayer)) {
        print("Starting to homogenize raster with baselayer")
        filename_2<-tempfile(pattern = "raster_",fileext = ".tif")
        gdalUtils::gdalwarp(
          srcfile = filename_1,
          dstfile = filename_2,
          tr = res(my_baselayer),
          te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                       " "),
          s_srs = proj4string(my_input),
          t_srs = proj4string(my_baselayer),
          r = resampling_method, # should this be max or mode or freely choosable?
          ot = my_datatype,
          co = c("COMPRESS=LZW")
        )
        tmp_raster <- raster::raster(filename_2)
      } else {
        tmp_raster <- raster::raster(filename_1)
      }
    } else{
      # without reclassification matrix, only rescale the raster if resolution and extent differ
      if (res(my_input) != res(my_baselayer) ||
          raster::extent(my_input) != raster::extent(my_baselayer)) {

        # raster::writeRaster(my_input,
        #             paste(tempdir(), "/tempreclassraster.tif", sep = ""))
        print("Starting to homogenize the raster with baselayer")
        filename_3<-tempfile(pattern = "raster_",fileext = ".tif")
        gdalUtils::gdalwarp(
          srcfile = my_input@file@name,
          dstfile = filename_3,
          tr = res(my_baselayer),
          te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =
                       " "),
          s_srs = proj4string(my_input),
          t_srs = proj4string(my_baselayer),
          r = resampling_method,
          ot = my_datatype,
          co = c("COMPRESS=LZW")
        )
        tmp_raster <- raster::raster(filename_3)
      } else{
        stop(
          'No differences between input and output data defined. Nothing to do',
          call. = F
        )
      }
    }
    return(tmp_raster)
  }
