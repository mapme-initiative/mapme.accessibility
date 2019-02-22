# function to create a friction input layer from vector data
#' @title acc_vec2fric
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
#' @export acc_vec2fric
#'

# define function
acc_vec2fric<-
  function(my_input,
           my_outputname,
           my_baselayer,
           my_speed=NULL,
           my_speedfield=NULL,
           my_outputpath=NULL){
    # Test that either a travel speed or speedfield is defined
    if(is.null(my_speed)==TRUE&is.null(my_speedfield)==TRUE){
      print("Please define either a travel speed in kmh for the whole polygon dataset or an attribute field inside the dataset that contains the speeds")
    }else{
      # test if an extent is provided
      if(class(my_baselayer)!="RasterLayer"){print("Please provide a valid raster as baselayer")
        }else{
            # test if input data is spatial
            if(!class(my_input)%in%c("SpatialPolygonsDataFrame","SpatialLinesDataFrame")){
              print("Please give the input data as an object of Class SpatialPolygonsDataFrame or SpatialLinesDataFrame")
              }else{
                # test whether a field name of the travel-speed field exists if this is given
                if(!is.null(my_speedfield)&&!my_speedfield%in%colnames(my_input@data)){
                  print("my_speedfield has to be given as a text-string containing the valid name of an attribute field in the data")
                  }else{
                    # test whether the field type of a given travelspeed field is numeric if one is given
                    if(is.null(my_speedfield)==F&is.numeric(test@data[,my_speedfield])==F){
                      print("The datatype of the travelspeed column has to be numeric")
                    }else{
                      # test wheter the value for the speed field is a number
                      if(!is.null(my_speed)&(!is.numeric(my_speed)|length(my_speed)!=1)){
                        print("my_speed has to be numeric of length 1")
                      }else{
                        # check that output path is defined
                        if(is.null(my_outputpath)){
                          print("please define a valid and existing output path for the raster data")
                        }else{
                          # check  wheter the output path exists
                          if(!dir.exists(my_outputpath)){
                            print("please define a valid and existing output path for the raster data")
                          }else{
                          # check that the libraries are installed
                          if(is.element("gdalUtils", installed.packages()[,1])==F){
                            print("You do not have galUtils installed. Please install the package before proceeding")
                          }else{
                            if(is.element("rgdal", installed.packages()[,1])==F){
                              print("You do not have rgdal installed. Please install the package before proceeding")
                            }else{
                              if(is.element("raster", installed.packages()[,1])==F){
                                print("You do not have raster installed. Please install the package before proceeding")
                              }else{
                                # load library
                                library("gdalUtils")
                                library("rgdal")
                                library("raster")
                                # start processing
                                print("Starting to convert the data")
                                # add the column and save data in output directory
                                tmp.data<-my_input
                                if(!is.null(my_speed)){tmp.data@data$accsp<-my_speed}
                                writeOGR(tmp.data,my_outputpath, my_outputname,"ESRI Shapefile")
                                print(paste("Saved input data as",my_outputname,"Shapefile in the folder",my_outputpath," for rasterization. Can be  removed afterwards."))
                                # get fieldname for rasterization
                                if(is.null(my_speedfield)){tmp.name<-accsp}else{tmp.name<-my_speedfield}
                                # rasterize
                                gdal_rasterize(src_datasource = paste(my_outputpath,"/",my_outputname,".shp",sep=""),
                                               a= my_speedfield,
                                               dst_filename = paste(my_outputpath,"/",my_outputname,".tif",sep=""),
                                               tr = res(my_baselayer),
                                               te= paste(extent(my_baselayer)[c(1,3,2,4)],collapse=" "),
                                               ot= "INT1U",
                                               a_nodata="none",
                                               co= c("COMPRESS=LZW",
                                                   "TFW=YES"))
                                # print final message and return resuls
                                print("Done processing!")
                                r.tmp<-raster(paste(my_outputpath,"/",my_outputname,".tif",sep=""))
                                return(r.tmp)
          }}}}}}}}}}}}




