#COLLAR DATA
#this script can convert activity counts to continues variable (batch)
#delete empty file at first!!!!!!

a <- list.files("data/tmp")
a
dir <- paste("data/tmp/", a, sep = "")
n <- length(dir)
n
merge.continues = data.frame( Date = NA, RTime = NA, X = NA, Y = NA)

for(i in 1:n){
  collar_data <- read.csv(file = dir[i], header = T, sep = ",", stringsAsFactors = F)
  head(collar_data)

  ##split date and time
  collar_data$Time <- NA
  collar_data$Date <- NA
  collar_data$time <- as.character(collar_data$时间)
  collar_data$act_count <- collar_data$运动量
  collar_data$X <- collar_data$经度
  collar_data$Y <- collar_data$纬度
  
  for(i in 1:nrow(collar_data)){
    collar_data$Date[i] <- strsplit(collar_data$time[i],split = " ",fixed=T)[[1]][1]
    collar_data$Time[i] <- strsplit(collar_data$time[i],split = " ",fixed=T)[[1]][2]
  }
  
  ##split hour and min
  collar_data$hour <- NA
  collar_data$min <- NA
  for(i in 1:nrow(collar_data)){
    collar_data$hour[i] <- strsplit(collar_data$Time[i],split = ":",fixed=T)[[1]][1]
    collar_data$min[i] <- strsplit(collar_data$Time[i],split = ":",fixed=T)[[1]][2]
  }
  
  ##convert time to circular statistics(0-2*pi)
  for(i in 1:nrow(collar_data)){
    hour1 <- as.numeric(collar_data$hour[i])
    min1 <- as.numeric(collar_data$min[i])
    collar_data$RTime[i] <- (hour1 + min1 / 60) * 2 * pi / 24
  }
  
  ##exclude data with a time difference greater than 3 hours
  ##exclude the first data  
  for(i in 1:nrow(collar_data)){
    if(i == 1){
      collar_data$mark[i] <- 0
    }
    else{
      date0 <- as.Date(collar_data$Date[i-1])
      date1 <- as.Date(collar_data$Date[i])
      time0 <- collar_data$RTime[i-1]
      time1 <- collar_data$RTime[i]
      time_interval <- (date1 - date0) * 2 * pi + (time1 - time0)
      
      if(time_interval > (3 * 2 * pi / 24)){
        collar_data$mark[i] <- 0
      }
      else{
        collar_data$mark[i] <- 1
      }
    }
  }
  
  ##sample     
  recordall = data.frame(Date = NA, RTime = NA, X = NA, Y = NA)
  for(i in 1:nrow(collar_data)){
    if(collar_data$mark[i] == 1){
      
      date0 <- as.Date(collar_data$Date[i-1])
      date1 <- as.Date(collar_data$Date[i])
      time0 <- collar_data$RTime[i-1]
      time1 <- collar_data$RTime[i]
      activity_count <- as.numeric(collar_data$act_count[i])
      
      if(time0 < time1){
        rnd_time <- sample(seq(time0, time1, ((1 / 60) * 2 * pi / 24)), 
                           size = activity_count, replace = T)
        
        tmp <- NA
        tmp <- data.frame(RTime = rnd_time, 
                          Date = rep(collar_data$Date[i], times=activity_count),
                          X = rep(collar_data$X[i], times=activity_count), 
                          Y = rep(collar_data$Y[i], times=activity_count))
        recordall <- rbind(recordall, tmp)
      }
      else{
        act0 = floor(activity_count * (2 * pi - time0) / (2 * pi + time1 - time0))
        act1 = activity_count - act0
        
        rnd_time0 <- sample(seq(time0, 2 * pi, ((1 / 60) * 2 * pi / 24)), 
                            size = act0, replace = T)
        rnd_time1 <- sample(seq(0, time1, ((1 / 60) * 2 * pi / 24)), 
                            size = act1, replace = T)
        
        tmp <- NA
        tmp <- data.frame(RTime = c(rnd_time0, rnd_time1), 
                          Date = c(rep(collar_data$Date[i-1], times=act0), 
                                   rep(collar_data$Date[i], times=act1)),
                          X = rep(collar_data$X[i], times=activity_count), 
                          Y = rep(collar_data$Y[i], times=activity_count))
        recordall <- rbind(recordall, tmp)
      }
      
    }
    print(i)
  }
  recordall <- recordall[-1,]
  
  merge.continues <- rbind(merge.continues, recordall)
}

merge.continues <- merge.continues[-1,]
nrow(merge.continues)
write.csv(merge.continues, file = "data/tmp/PKU004_NG_point.csv")
