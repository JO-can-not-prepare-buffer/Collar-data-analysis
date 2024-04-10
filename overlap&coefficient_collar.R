########################################################################
# overlap of two different data
# ATTENTION
# Change the date format of {Date} to "Date" in your excel before input
########################################################################


####library####
# install.packages('activity')

library(overlap)
library(activity)
library(sp)

####input data####

collar1 <- read.csv("data/activity/10w_All_femaleG.csv",stringsAsFactors = F)
head(collar1)

collar2 <- read.csv("data/activity/10w_All_femaleNG.csv",stringsAsFactors = F)
head(collar2)


####collar1####
collar1_Date <- as.POSIXct(collar1$Date, tz = Sys.timezone())
collar1_X <- mean(collar1$X); collar1_Y <- mean(collar1$Y)

collar1_Coord <- matrix(c(collar1_X, collar1_Y), nrow = 1)
collar1_Coord <- sp::SpatialPoints(collar1_Coord, 
                                  proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

collar1$SunTime <- sunTime(collar1$RTime, collar1_Date, collar1_Coord)
densityPlot(collar1$random_points_sample, ylim=c(0,0.15), main = "city")

rm(collar1_X)
rm(collar1_Y)
rm(collar1_Coord)




####collar2####
collar2_Date <- as.POSIXct(collar2$Date, tz = Sys.timezone())
collar2_X <- mean(collar2$X); collar2_Y <- mean(collar2$Y)

collar2_Coord <- matrix(c(collar2_X, collar2_Y), nrow = 1)
collar2_Coord <- sp::SpatialPoints(collar2_Coord, 
                                   proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

collar2$SunTime <- sunTime(collar2$RTime, collar2_Date, collar2_Coord)
densityPlot(collar2$SunTime, ylim=c(0,0.08), main = "mount")

rm(collar2_X)
rm(collar2_Y)
rm(collar2_Coord)




####overlapEst for collar1 and 2####

overlap12 <- overlapEst(collar1$SunTime,
                             collar2$SunTime,
                             type = "Dhat4")
overlap12


##confidence interval
Co12_BS <- bootstrap(collar1$SunTime,
                        collar2$SunTime, 100,
                        type = "Dhat4")
bootCI(overlap12, Co12_BS)

##Wald test
fco1 <- fitact(collar1$SunTime)
fco2 <- fitact(collar2$SunTime)
Co12_comp <- compareCkern(fco1, fco2, reps = 10)
Co12_comp


overlapPlot(collar1$SunTime,
            collar2$SunTime,
            main="", xlab="",
            ylab="", ylim=c(0,0.08), las = 1)

legend(0, 0.13, c(" ", " "), lty = c(1, 2), col = c(1, 4), bty = 'n')
