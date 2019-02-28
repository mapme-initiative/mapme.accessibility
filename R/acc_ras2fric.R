# function to create a friction input layer from vector data
#' @title acc_ras2fric
#'
#' @description Function to convert vector to friction base data
#'
#' @param my_input
#' @param my_outputpath
#' @param my_outputname
#' @param my_baselayer
#' @param my_reclassmatrix
#' @param my_speedfield
#' @param my_outputpath
#'
#' @return tmp_raster
#'
#' @examples NULL
#'
#' @export acc_ras2fric
#'

# define function
acc_ras2fric <-
  function(my_input_path,
           my_outputpath,
           my_outputname,
           my_baselayer,
           my_reclass_inputvalues = NULL,
           my_reclass_outputvalues = NULL)
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
          # check wheter the reaser package is installed
          if (is.element("raster", installed.packages()[, 1]) == F) {
            print(
              "You do not have 'raster' installed. Please install the package before proceeding"
            )
          } else{
            # check wheter gdalUtils is installed
            if (is.element("gdalUtils", installed.packages()[, 1]) == F) {
              print(
                "You do not have 'galUtils' installed. Please install the package before proceeding"
              )
            } else{
              # start processing
              print("Starting to process the data")
              library("raster")
              library("gdalUtils")
              tmp_raster<-raster(my_input_path)
              # check  wheter the data should be reclassified
              if (!is.null(my_reclass_inputvalues)) {
                print("You provided a reclassification matrix. Starting to reclassify the raster")
                # reclassify
                reclassify(
                  tmp_raster,
                  cbind(my_reclass_inputvalues,my_reclass_outputvalues),
                  include.lowest = T,
                  filename = paste(my_outputpath,
                                   my_outputname,
                                   "_reclass.tif", sep =""),
                  datatype = "INT1U",
                  options = c("COMPRESS=LZW"))
                # print output
                print(paste("reclassified raster saved as ",paste(my_outputpath,
                                                                    my_outputname,
                                                                    "_reclass.tif", sep =""),". It can be safely removed afterwards"))
                  # rescale the raster if resolution and or extent differs
                if(res(tmp_raster)!=res(my_baselayer)&&extent(tmp_raster)!=extent(my_baselayer)){
                  print("Starting to rescale and aling the raster")
                  gdalwarp(srcfile = paste(my_outputpath,
                                           my_outputname,
                                           "_reclass.tif", sep =""),
                           dstfile = paste(my_outputpath,
                                           my_outputname,
                                           ".tif", sep =""),
                           tr = res(my_baselayer),
                           te = paste(extent(my_baselayer)[c(1,3,2,4)],collapse=" "),
                           r ="max",
                           ot= "Byte",
                           overwrite=F)
                  tmp.confirmation<-"Reclass and Rescale"
                }else{tmp.confirmation<-"Reclass"}

              } else{
                # without reclassification matrix, only rescale the raster if resolution and extent differ
                if(res(tmp_raster)!=res(my_baselayer)&&extent(tmp_raster)!=extent(my_baselayer)){
                  print("Starting to rescale and aling the raster")
                  gdalwarp(srcfile = my_input_path,
                           dstfile = paste(my_outputpath,
                                           my_outputname,
                                           ".tif", sep =""),
                           tr = res(my_baselayer),
                           te = paste(extent(my_baselayer)[c(1,3,2,4)],collapse=" "),
                           r ="max",
                           ot= "Byte",
                           overwrite=F)
                  tmp.confirmation<-"Rescale"
                }else{
                  print("No differences between input and output. Nothing to do")
                  tmp.confirmation<-"nothing done"
                  }

              }
              # return results depending on what was done
              # only reclassifcation
              if(tmp.confirmation=="Reclass"){
                tmp_raster<-raster(paste(my_outputpath,
                                  my_outputname,
                                  "_reclass.tif", sep =""))
                print(paste("Done processing. Result saved as ",my_outputpath,
                                                                my_outputname,
                                                                "_reclass.tif", sep =""))
              }else{
                # reclassificationa and rescale
                if(tmp.confirmation=="Reclass and Rescale"){
                  tmp_raster<-raster(paste(my_outputpath,
                                           my_outputname,
                                           ".tif", sep =""))
                  print(paste("Done processing. Result saved as ",
                              my_outputpath,
                              my_outputname,
                              ".tif", sep =""))
                }else{
                  tmp_raster<-NULL
                  print("nothing really done")
                }
              }
              return(tmp_raster)
            }
          }
        }
      }
    }
  }
