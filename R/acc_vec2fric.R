# function to create a friction input layer from vector data
#' @title acc_vec2fric
#'
#' @description Function to convert vector to friction base data
#'
#' @param my_input
#' @param my_baselayer
#' @param my_speed
#' @param my_speedfield
#' @param my_datatype
#'
#' @return r_tmp
#'
#' @examples NULL
#'
#' @export acc_vec2fric
#'

# define function
acc_vec2fric <-
  function(my_input,
           my_baselayer,
           my_speed = NULL,
           my_speedfield = NULL,
           my_datatype = "UInt16") {
    # Check for correct definition of input variables
    if (!inherits(my_baselayer,c("RasterLayer"))) {
      stop('Please provide "my_baselayer" as an object of Class "RasterLayer".',
           call. = F)}
    if (!inherits(my_input,c("Spatial"))) {
      stop('Please provide "my_input" as an object of Class "Spatial"(sp) e.g. a "SpatialLinesDataframe"',
           call. = F)}
    if (!is.null(my_speed)&&!inherits(my_speed,c("numeric","integer"))&&!length(my_speed)==1) {
      stop('Please provide "my_speed" as a single integer or numeric.' ,
           call. = F)}
    if (!is.null(my_speedfield)&&!is.element(my_speedfield,colnames(my_input@data))) {
      stop(paste("Could not find",my_speedfield,"in my_input@data. Please provide a valid field name") ,
           call. = F)}
    if (!is.null(my_speedfield)&&!is.null(my_speed)) {
      stop('You have to either specify a valid travel speed for all features with "my_speed" or a valid field name containing travelspeeds with "my_speedfield"' ,
           call. = F)}
    ## start processing
    # add the column and save data in the temp directory
    tmp_data <- my_input
    if (!is.null(my_speed)) {
      tmp_data@data$accsp <- my_speed
    } else {
      tmp_data@data$accsp <- tmp_data@data[,my_speedfield]
    }
    # reproject if necessary
    if (sp::proj4string(tmp_data)!=sp::proj4string(my_baselayer)){
      tmp_data<-spTransform(tmp_data,CRSobj = CRS(proj4string(my_baselayer)))
    }
    tmp_name<-gsub("/","",tempfile(pattern="tempvector",tmpdir = ""))
    rgdal::writeOGR(tmp_data,
                    tempdir(),
                    tmp_name,
                    "ESRI Shapefile")
    # create tempname
    tmp_name_raster<-tempfile(pattern = "raster_",fileext = ".tif")
    # rasterize
    gdalUtils::gdal_rasterize(
      src_datasource = paste(tempdir(),"/",tmp_name, ".shp", sep =""),
      a = "accsp",
      dst_filename = tmp_name_raster,
      tr = res(my_baselayer),
      te = paste(extent(my_baselayer)[c(1, 3, 2, 4)], collapse =" "),
      ot = my_datatype,
      co = c("COMPRESS=LZW"),
      a_nodata = "none"
    )
    r_tmp<-raster(tmp_name_raster)
    # return results and delete vectordata
    unlink(c(paste(tempdir(), "/tempvector.*", sep ="")))
    return(r_tmp)
  }
#test
