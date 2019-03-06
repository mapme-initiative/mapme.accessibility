\name{acc_slopecorr}
\alias{acc_slopecorr}
\title{acc_slopecorr}
\usage{
  acc_slopecorr(my_input, my_radians)
}
\description{
  This function corrects an input raster fcontaining travel speed for slope effects. The conversion is done with the function my_input * (exp(-3 * tan(my_radians)))
}
\arguments{
  item{my_input}{Input layer containing travelspeeds for slope correction. Should be of type raster(raster)}
  item{my_radians}{Input layer containing slope measured in radians for correction.  Should be of type raster(raster) and share same extent, resolution and reference system as the travelspeed input layer. Can be created with the acc_radians function}
}

\examples{
  acc_slopecorr()
}
