#' @title acc_accessibility
#'
#' @description Function to calculate accessiblity map
#'
#' @param my_friction
#' @param my_sources
#' @param knightsmove
#' @param output_CRS
#' @param grassbin
#' @param max_ram
#'
#' @return tmp_accessibiltiy
#'
#' @examples NULL
#'
#' @export acc_accessibility
#'

# define function
acc_accessibility <-
  function(my_friction,
           my_sources,
           knightsmove = TRUE,
           grassbin="/usr/lib/grass72/",
           max_ram=3000) {
    # check package availability before proceeding
    if (is.element("raster", installed.packages()[, 1]) == F) {
      print(
        "You do not have 'raster' installed. Please install the package before proceeding"
        )
    } else{
      if (is.element("sp", installed.packages()[, 1]) == F) {
        print(
          "You do not have 'sp' installed. Please install the package before proceeding"
        )
      } else{
        if (is.element("rgrass7", installed.packages()[, 1]) == F) {
          print(
            "You do not have 'rgrass7' installed. Please install the package before proceeding"
          )
          else{
            library("raster")
            library("sp")
            library("rgrass7")
            # (1) Import data
            print("Setting up grass environment and import data")
            # Create a new GrassDB, set location and mapset
            loc <- initGRASS(grassbin,
                             home=tempdir(),
                             gisDbase = "grassdb",
                             location = "grassloc",
                             mapset = "grassmapset",
                             override=TRUE)
            # Import friction raster to GRASS
            writeRaster(x = my_friction,filename = paste(tempdir(),"/friction.tif"))
            execGRASS("r.in.gdal",
                      flags=c("overwrite","o"),
                      parameters=list(input=paste(tempdir(),"/friction.tif"),
                                      output="friction"))
            # set grass region from raster
            execGRASS("g.region",
                      parameters=list(raster="friction"))
            # save input layer and reproject sources crs to match raster crs (if necessary)
            if(proj4string(my_sources)!=proj4string(my_friction)){
            writeOGR(obj = spTransform(my_sources,CRSobj = CRS(proj4string(my_friction))),dsn = tempdir(),layer = "sources",driver = "ESRI Shapefile",overwrite_layer = T)
            }else{writeOGR(obj = my_sources, dsn = tempdir(),layer = "sources",driver = "ESRI Shapefile",overwrite_layer = T)}
            # import sources
            execGRASS("v.in.ogr",
                      flags=c("r","overwrite","o"), # r limits to the current mapregion, o skips projection check -> make sure it is the same was output raster
                      parameters=list(input=paste(tempdir(),"/sources.shp",sep=""), output="sources"))
            # calculate accessibility
            print(paste("Starting to calculate accessiblity map at:"),Sys.time())
            if (knightsmove==TRUE){
            execGRASS("r.cost",
                      flags=c("k","overwrite"), # k forces to use knightsmove, slower but more accurate
                      parameters=list(input="friction",
                                      start_points="sources",
                                      output="accessibility",
                                      memory=max_ram))
            }else{
              execGRASS("r.cost",
                        flags=c("overwrite"), # no nightsmove
                        parameters=list(input="friction",
                                        start_points="sources",
                                        output="accessibility",
                                        memory=max_ram))
            }
            print(paste("Stopped to calculate accessiblity map at:"),Sys.time())
            # Export raster as tif
            execGRASS("r.out.gdal",
                      flags="overwrite",
                      parameters=list(input="accessibility",
                                      output=paste(tempdir(),"accessibility.tif",sep="")
                      )
            )
            tmp_accessibiltiy<-raster(paste(tempdir(),"accessibility.tif",sep=""))
            return(tmp_accessibiltiy)
          }



        }
      }
    }
  }
