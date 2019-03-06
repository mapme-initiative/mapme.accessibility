\name{acc_radians}
\alias{acc_radians}
\title{acc_radians}
\usage{
  acc_radians(
  my_input,
  my_baselayer
  )
}
\description{
  This function creates a slopemap in radians and homogenizes it with the baselayer
}
\arguments{
  \item{my_input}{An input layer containing a Digital Elevation model DEM. Should be of type raster(raster)}
\item{my_baselayer}{A baselayer that defines resolution, extent and projection system of the friction map. Should be of class raster(raster). Most commonly a layer containing land-use values. Can be the same layer as the input layer}
}

\examples{
  acc_radians()
}
