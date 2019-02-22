# function to create a friction input layer from vector data
#' @title acc_ras2fric
#'
#' @description Function to convert vector to friction base data
#'
#' @param my_input
#' @param my_outputname
#' @param my_baselayer
#' @param my_speed
#' @param my_speedfield
#' @param my_outputpath
#'
#' @return r.tmp
#'
#' @examples NULL
#'
#' @export acc_ras2fric
#'

# define function
acc_ras2fric <-
  function(my_input_path,
           my_outputname,
           my_baselayer,
           my_reclassmatrix = NULL,
           my_outputpath = NULL)
  {
    # test if baselayer data is a raster
    if (class(my_baselayer) != "RasterLayer") {
      print("Please provide my_baselayer as an object of Class RasterLayer")
    } else{
      # check that output path is defined
      if (is.null(my_outputpath)) {
        print("please define a valid and existing output path for the raster data")
      } else{
        # check  wheter the output path exists
        if (!dir.exists(my_outputpath)) {
          print("please define a valid and existing output path for the raster data")
        } else{
          if (is.element("raster", installed.packages()[, 1]) == F) {
            print(
              "You do not have raster installed. Please install the package before proceeding"
            )
          } else{
            if (is.element("gdalUtils", installed.packages()[, 1]) == F) {
              print(
                "You do not have galUtils installed. Please install the package before proceeding"
              )
            } else{
              # start processing
              print("Starting to convert the data")
              # check  wheter the data should be reclassified
              if (!is.null(my_reclassmatrix)) {
                print("Reclassify the raster")
                  reclassify(
                    raster(my_input_path),
                    my_reclassmatrix,
                    include.lowest = T,
                    filename = paste(my_outputpath,
                                     my_outputname,
                                     "reclass.tif", sep =""),
                    datatype = "INT1U",
                    options = c("COMPRESS=LZW"))
                  print(paste("reclassified raster saved as ",paste(my_outputpath,
                                                                    my_outputname,
                                                                    "reclass.tif", sep =""),". It can be safely removed afterwards"))
                  # rescale the raster
                  print("rescale the raster")
                  gdalwarp(srcfile = paste(my_outputpath,
                                           my_outputname,
                                           "reclass.tif", sep =""),
                           dstfile = paste(my_outputpath,
                                           my_outputname,
                                           ".tif", sep =""),
                           tr = res(my_baselayer),
                           te = paste(extent(my_baselayer)[c(1,3,2,4)],collapse=" "),
                           r ="max",
                           ot= "Byte",
                           overwrite=F)

              } else{
                # rescale the raster
                gdalwarp(srcfile = my_input_path,
                         dstfile = paste(my_outputpath,
                                         my_outputname,
                                         ".tif", sep =""),
                         tr = res(my_baselayer),
                         te = paste(extent(my_baselayer)[c(1,3,2,4)],collapse=" "),
                         r ="max",
                         ot= "Byte",
                         overwrite=F)
              }
            }
          }
        }
      }
    }
  }
