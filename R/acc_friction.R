# function to create a friction input layer from vector data
#' @title acc_friction
#'
#' @description Function to create a friction surface
#'
#' @param my_friction_layer_list
#' @param my_outputresolution
#' @param getproj
#' @param my_proj
#' @param cropfriction
#' @param my_croplayer
#'
#' @return tmp_friction
#'
#' @examples NULL
#'
#' @export acc_friction
#'


# define function
acc_friction <- function(my_friction_layer_list,
                        my_outputresolution,
                        getproj = TRUE,
                        my_proj = NULL,
                        cropfriction = FALSE,
                        my_croplayer = NULL) {
  # check for correct definition of input variables
  if (!inherits(my_friction_layer_list, c("list"))) {
    stop('Please provide "my_friction_layer_list" as an object of Class "list".',
         call. = F)
  }
  if (getproj==FALSE && !inherits(my_proj, "character")) {
    stop('You decided to provide a projection system. Please provide "my_proj" as an object of Class "character" containing a valid proj4string definition.',
         call. = F)
  }
  if (getproj==TRUE && !is.null(my_proj)) {
    stop('You decided to provide a custom projection system. Please set "getproj" to FALSE',
         call. = F)
  }
  if (!is.null(my_croplayer) && inherits(my_proj, c("Spatial"))) {
    stop('You decided to provide a layer to crop your data. Please provide "my_croplayer" as an object of Class "SpatialPolygonsDataFrame".',
         call. = F)
  }
  if (!is.null(my_croplayer) && cropfriction==FALSE) {
    stop('You decided to provide a layer to crop your data. Please set cropfriction = TRUE to proceed',
         call. = F)
  }
  # stack the layers
  tmp_stack <- raster::stack(unlist(my_friction_layer_list))
  # create tempfilename
  filename_1<-tempfile(pattern = "raster_",fileext = ".tif")
  # take max
  tmp_friction <- raster::stackApply(
    tmp_stack,
    indices = rep(1, length(tmp_stack@layers)),
    fun = max,
    filename = filename_1,
    datatype = "FLT4S"
  )
  # create tempfilename
  filename_2<-tempfile(pattern = "raster_",fileext = ".tif")
  # project the raster
  if (getproj == TRUE) {
    tmp_proj <- acc_areaproj(tmp_friction)
  } else{
    tmp_proj <- my_proj
  }
  gdalUtils::gdalwarp(
    srcfile = filename_1,
    dstfile = filename_2,
    of = "GTiff",
    tr = c(my_outputresolution, my_outputresolution),
    s_srs = proj4string(tmp_friction),
    t_srs = tmp_proj,
    dstnodata = -9999 # is this needed?
  )
  # create tempfilename
  filename_3<-tempfile(pattern = "raster_",fileext = ".tif")
  # create traveltimes in seconds to cross one cell
  tmp_friction <-
    raster(filename_2)
  tmp_friction <-
    raster::calc(tmp_friction,
                 function(x) {
                   my_outputresolution / ((x * 1000) / 3600)
                 },
                 filename = filename_3,
                 datatype = "INT4U",
                 options = c("COMPRESS=LZW"))
  # if user decides to crop the friction layer, crop it
  if (cropfriction == TRUE) {
    # (1) reproject the crop layer to match PCS
    # create tempfilename
    filename_4<-gsub("/","",tempfile(pattern="tempvector",tmpdir = ""))
    tmp_croplayer <-
      spTransform(my_croplayer, CRSobj = CRS(proj4string(tmp_friction)))
    writeOGR(obj = tmp_croplayer,
             dsn = tempdir(),
             layer = filename_4,
             "ESRI Shapefile")
    # create tempfilename
    filename_5<-tempfile(pattern = "raster_",fileext = ".tif")
    # (2) crop
    gdalwarp(
      srcfile = filename_3,
      dstfile = filename_5,
      cutline = paste(tempdir(),"/",filename_4,".shp", sep = ""),
      crop_to_cutline = TRUE,
      dstnodata = -9999
    )
    tmp_friction <-
      raster(filename_5)
  }
  return(tmp_friction)
}
