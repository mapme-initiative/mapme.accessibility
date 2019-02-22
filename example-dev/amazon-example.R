r.test<-acc_vec2fric(my_input = test,
                     my_outputname = "test2",
                     my_speedfield = "sup_00",
                     my_baselayer = r.friction,
                     my_outputpath = "output/test/")


test.r<-raster("/home/jschielein/R/R-projects/3_accessibility/puplication/accessibility_publication/input/landcover/lc_am_30m.tif")
test.m<-cbind(c(33,31,3,4,5,9,27,11,12,13,15,19,20,21,23,24,25,29,30,32),
              c(14,6,3,3,3,3,3,15,15,15,15,15,15,15,15,15,15,15,15,15))
acc_ras2fric(my_input_path = "/home/jschielein/R/R-projects/3_accessibility/puplication/accessibility_publication/input/landcover/lc_am_30m.tif",
             my_outputname = "test",
             my_baselayer = test.r,
             my_reclassmatrix = test.m,
             my_outputpath = "/home/jschielein/R/R-projects/3_accessibility/test/"
)
