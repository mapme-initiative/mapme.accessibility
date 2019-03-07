\name{acc_slopecorr}
\alias{acc_slopecorr}
\title{acc_slopecorr}
\usage{
  acc_slopecorr(
  my_input,
  my_radians
  )
}
\description{
  This function corrects an input raster containing travel speed for slope effects. The conversion is done with the function my_input * (exp(-3 * tan(my_radians)))
}
\arguments{
  \item{my_input}{An iput layer containing travelspeeds for slope correction. Should be of type raster(raster)}
  \item{my_radians}{An input layer containing slope measured in radians for correction.  Should be of type raster(raster) and share same extent, resolution and reference system as the travelspeed input layer. Can be created with the acc_radians function}
  \item{correctionfactor}{This is the correction factor which defines the strength of slope correction. Default value is 3. see...}
}

\examples{
  acc_slopecorr()
}
