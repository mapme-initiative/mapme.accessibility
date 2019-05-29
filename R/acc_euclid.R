#' @title acc_euclid
#'
#' @description Function to calculate euclidean distances to sources for a raster map.
#'
#' @param my_sources_raster
#' @param my_metric
#' @param grassbin
#'
#' @return tmp_accessibiltiy
#'
#' @examples NULL
#'
#' @export acc_euclid
#'

# define function
acc_euclid <-
  function (my_sources_raster,
            grassbin,
            my_metric = "euclidean")
  {
    print("Setup grass environment and import data")
    filename_1 <- tempfile(pattern = "gisDbase_")
    loc <-
      initGRASS(
        grassbin,
        home = tempdir(),
        gisDbase = "grassdb",
        location = "grassloc",
        mapset = "grassmapset",
        override = TRUE
      )
    filename_2 <- tempfile(pattern = "raster_", fileext = ".tif")
    writeRaster(x = my_sources_raster, filename = filename_2)
    execGRASS(
      "r.in.gdal",
      flags = c("overwrite", "o"),
      parameters = list(input = filename_2,
                        output = "my_sources_input")
    )
    execGRASS("g.region", parameters = list(raster = "my_sources_input"))
    print(paste("Started calcuation of euclidean distance map at:",
                Sys.time()))
    execGRASS(
      "r.grow.distance",
      flags = c("overwrite"),
      parameters = list(
        input = "my_sources_input",
        distance = "euclid",
        metric = my_metric
      )
    )
    print(paste("Stopped calcuation of euclidean distance map at:",
                Sys.time()))
    filename_4 <- tempfile(pattern = "raster_", fileext = ".tif")
    execGRASS(
      "r.out.gdal",
      flags = c("overwrite"),
      parameters = list(input = "euclid",
                        output = filename_4)
    )
    tmp_accessibiltiy <- raster(filename_4)
    tmp_accessibiltiy@crs <- sp::CRS(proj4string(my_sources_raster))
    return(tmp_accessibiltiy)
  }
