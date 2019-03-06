\name{acc_radians}
\alias{acc_radians}
\title{acc_radians}
\usage{
  acc_radians(my_input, my_baselayer)
}
\description{
  This function creates a slopemap in radians and homogenizes it with the baselayer
}
\arguments{
  item{my_input}{Input layer containing a Digital Elevation model DEM. Should be of type raster(raster)}
  item{my_baselayer}{A baselayer of class "raster" that defines resolution, extent and projection system of the friction map. Most commonly a raster containing land-use values.}
}

\examples{
  acc_radians()
}
