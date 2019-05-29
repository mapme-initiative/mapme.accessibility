\name{acc_euclid}
\alias{acc_euclid}
\title{acc_euclid}
\usage{
  acc_euclid(
  my_sources_raster,
  grassbin,
  my_metric = "euclidean"
  )
}
\description{This is a wrapper function to use rgrass7 to calculate an euclidean distance map with the r.grow.distance function.

}
\arguments{
  \item{my_sources_raster}{This should be a layer containing the sources. Should be of type raster(raster). Can be created using the acc_vec2fric function on a vector layer}
    \item{my_metric}{Distance metric to be used. Defaults to "euclidean". Can be also squared|maximum|manhattan|geodesic. For details see https://grass.osgeo.org/grass72/manuals/r.grow.distance.html}
  \item{grassbin}{Define here the path to the grass binaries. An easy way to find the binaries is to use the "findGRASS" function from the the package "link2GI". Consider installing it if you do not know the GRASS location and are unable to find it manually. Additional information can be found here: https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7#GRASS_within_R. If you encounter an error regarding iconv.dll or other configuration errors for GRASS on Windows, this page might help: https://cran.r-project.org/web/packages/openSTARS/vignettes/Warnings_and_Errors.html}

}

\examples{
# Please refer to https://s5joschi.github.io/accessibility/ to see usage examples.
}
