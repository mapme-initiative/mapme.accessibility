\name{acc_vec2fric}
\alias{acc_vec2fric}
\title{acc_vec2fric}
\usage{
  acc_vec2fric(my_input, my_baselayer, my_speed = NULL, my_speedfield = NULL)
}
\description{
  This function converts vector data into a raster layer containing travel speeds that can be used to create a friction map.
}
\arguments{
item{my_input}{Vector input layer for the conversion. Should be of type SpatialPolygons(sp)}
item{my_baselayer}{A baselayer of class "raster" that defines resolution, extent and projection system of the friction map. Most commonly a raster containing land-use values}
item{my_speed}{Travel speed in km/h that applies to all polygons in this layer. If the travelspeed of polygons differs depending on a certain feature (e.g. paved/unpaved roads) use my_speedfield instead}
item{my_speedfield}{column name of the input layer that contains travel speeds for the different polygons in km/h}
}

\examples{
  acc_vec2fric()
}
