\name{acc_accessibility}
\alias{acc_accessibility}
\title{acc_accessibility}
\usage{
  acc_accessibility(
  my_friction,
  my_sources,
  knightsmove = TRUE,
  grassbin,
  max_ram = 3000
  )
}
\description{This is a wrapper function to use rgrass7 to calculate an accessibility map using the r.cost function.

}
\arguments{
  \item{my_friction}{Friction map for the calculations. Should be of type raster(raster)}
  \item{my_sources}{A layer containing the destinies for the accessibility map. Should be of type SpatialPolygons(sp)}
  \item{knightsmove}{Use the knightsmove for accessibility calculation? More accurate but also slower. See r.cost document of GRASS for more details. Default value is TRUE.}
    \item{max_ram}{Define the maxiumum amount of RAM to be used for the calculation. Should not exeed your available RAM for this. Generally about 30 to 50 \% of total RAM should be okay. Default value is 3000 which is suitable for computers with 8GB of RAM}
  \item{grassbin}{Define here the path to the grass binaries. An easy way to find the binaries is to use the "findGRASS" function from the the package "link2GI". Consider installing it if you do not know the GRASS location and are unable to find it manually. Additional information can be found here: https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7#GRASS_within_R. If you encounter an error regarding iconv.dll or other configuration errors for GRASS on Windows, this page might help: https://cran.r-project.org/web/packages/openSTARS/vignettes/Warnings_and_Errors.html}

}

\examples{
  acc_accessibility()
}
