#####
# function to Calculate day or night and add column for collar data
# kyq 202204
## day: sunrise-sunset
## night: sunset-sunrise 

#####
day_night <- function(Data, X, Y){
  
  Data$time <- NA
  Data$Time <- NA
  Data$Date <- NA
  Data$time <- as.character(Data$t_)
  
  for(i in 1:nrow(Data)){
    Data$Date[i] <- strsplit(Data$time[i],split = " ",fixed=T)[[1]][1]
    Data$Time[i] <- strsplit(Data$time[i],split = " ",fixed=T)[[1]][2]
  }
  
  Data$hour <- NA
  Data$min <- NA
  for(i in 1:nrow(Data)){
    Data$hour[i] <- strsplit(Data$Time[i],split = ":",fixed=T)[[1]][1]
    Data$min[i] <- strsplit(Data$Time[i],split = ":",fixed=T)[[1]][2]
  }
  
  library(overlap)
  Data$RTime <- (as.numeric(Data$hour) + as.numeric(Data$min) / 60) * 2 * pi / 24
  Date <- as.POSIXct(Data$Date, tz = Sys.timezone())
  
  Coord <- matrix(c(X, Y), nrow = 1)
  Coord <- sp::SpatialPoints(Coord, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
  Data$SunTime <- sunTime(Data$RTime, Date, Coord)
  head(Data$SunTime)
  
  a <- 0.5*pi
  b <- 1.5*pi
  
  Data$DayOrNight <- NA
  for(i in 1:nrow(Data)){
    if((Data$SunTime[i]>a)&(Data$SunTime[i]<b)){
      Data$DayOrNight[i] <- "Day"
    }else{
      Data$DayOrNight[i] <- "Night"
    }
  }
  return(Data)
}