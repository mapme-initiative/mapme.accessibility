\name{acc_accessibility}
\alias{acc_accessibility}
\title{acc_accessibility}
\usage{
  acc_accessibility(my_friction, my_sources, knightsmove = TRUE, grassbin , max_ram = 3000)
}
\description{This is a wrapper function to use rgrass7 to calculate an accessibility map using the r.cost function.

}
\arguments{
  item{my_friction}{Friction map for the calculations. Should be of type raster(raster)}
  item{my_sources}{Destinies for the accessibility map. Should be of type SpatialPolygons(sp)}
  item{knightsmove}{Use the knightsmove for accessibilit calculation? More accurate but also slower. See r.cost document of GRASS for more details. Default value is TRUE.}
  item{grassbin}{Define here the path to the grass binaries. In Linux Environments typically "/usr/lib/grass72/"}
  item{max_ram}{Define the maxiumum amount of RAM to be used for the calculation. Should not exeed your available RAM for this. Generally about 30 to 50 percent of total RAM should be okay. Default value is 3000 which is suitable for computers with 8GB of RAM}
}

\examples{
  acc_accessibility()
}
