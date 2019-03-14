#' @title acc_accessibility
#'
#' @description Function to calculate accessiblity map from friction and input sources.
#'
#' @param my_friction
#' @param my_sources
#' @param knightsmove
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
           grassbin,
           max_ram = 3000) {
    # (1) Import data
    print("Setting up grass environment and import data")
    # Create a new GrassDB, set location and mapset
    filename_1 <- tempfile(pattern = "gisDbase_")

    loc <- initGRASS(
      grassbin,
      home = tempdir(),
      gisDbase = "grassdb",
      location = "grassloc",
      mapset = "grassmapset",
      override = TRUE
    )

    # create tempname and save friction raster to temp
    filename_2 <- tempfile(pattern = "raster_", fileext = ".tif")
    writeRaster(x = my_friction,
                filename = filename_2)
    # Import friction raster to GRASS
    execGRASS(
      "r.in.gdal",
      flags = c("overwrite", "o"),
      parameters = list(input = filename_2,
                        output = "friction")
    )
    # set grass region from raster
    execGRASS("g.region",
              parameters = list(raster = "friction"))
    # save input layer and reproject sources crs to match raster crs (if necessary)
    filename_3 <-
      gsub("/", "", tempfile(pattern = "tempvector", tmpdir = ""))
    if (proj4string(my_sources) != proj4string(my_friction)) {
      writeOGR(
        obj = spTransform(my_sources, CRSobj = CRS(proj4string(my_friction))),
        dsn = tempdir(),
        layer = filename_3,
        driver = "ESRI Shapefile",
        overwrite_layer = T
      )
    } else{
      writeOGR(
        obj = my_sources,
        dsn = tempdir(),
        layer = filename_3,
        driver = "ESRI Shapefile",
        overwrite_layer = T
      )
    }
    # import sources
    execGRASS(
      "v.in.ogr",
      flags = c("r", "overwrite", "o"),
      # r limits to the current mapregion, o skips projection check -> make sure it is the same was output raster
      parameters = list(
        input = paste(filename_3, ".shp", sep = ""),
        output = "sources"
      )
    )
    # calculate accessibility
    print(paste("Starting to calculate accessiblity map at:", Sys.time()))
    if (knightsmove == TRUE) {
      execGRASS(
        "r.cost",
        flags = c("k", "overwrite"),
        # k forces to use knightsmove, slower but more accurate
        parameters = list(
          input = "friction",
          start_points = "sources",
          output = "accessibility",
          memory = max_ram
        )
      )
    } else{
      execGRASS(
        "r.cost",
        flags = c("overwrite"),
        # no nightsmove
        parameters = list(
          input = "friction",
          start_points = "sources",
          output = "accessibility",
          memory = max_ram
        )
      )
    }
    print(paste("Stopped to calculate accessiblity map at:", Sys.time()))
    # Export raster as tif
    filename_4 <- tempfile(pattern = "raster_", fileext = ".tif")
    execGRASS(
      "r.out.gdal",
      flags = "overwrite",
      parameters = list(input = "accessibility",
                        output = filename_4)
    )
    tmp_accessibiltiy <-
      raster(filename_4)
    tmp_accessibiltiy@crs <- sp::CRS(proj4string(my_friction))
    return(tmp_accessibiltiy)
  }
