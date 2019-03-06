\name{acc_friction}
\alias{acc_friction}
\title{acc_friction}
\usage{
  acc_friction(
  my_friction_layer_list,
  my_outputresolution,
  getproj = TRUE,
  my_proj = NULL,
  cropfriction = FALSE,
  my_croplayer = NULL
  )
}
\description{
  This function creates a friction map from several friction input layers that contain travelspeeds
}
\arguments{
  \item{my_friction_layer_list}{List that contains all friction input layers. Those will be stacked and should therefore be all of class raster(raster) and share the same reference system, resolution as well as extent}
  \item{my_outputresolution}{This defines the output resolution for the friction map in meters.}
  \item{getproj}{Automatically retrive an appropriate geographic projection system for the region? If set to TRUE a projection system is generated in Lambert Azimutal Equal Area which is appropriate for larger areas.}
  \item{my_proj}{If "getproj" is set to FALSE this option enables users to pass a custom geographic projection system for the resarch area in form of a a string (proj4string)}
  \item{cropfriction}{should the final map be cropped with another layer? This is usefull to reduce processing time of the accessiblity calculation in the the acc_accessibility function.}
  \item{my_croplayer}{if cropfriction is set to TRUE, provide here a vector layer for cropping the friction map. Should be of type SpatialPolygons(sp)}
}

\examples{
  acc_friction()
}
