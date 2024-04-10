#daily landcover
#data: 3h interval collar data with landcover
#kyq202303

library(dplyr)
library(reshape2)
library(ggplot2)

a <- list.files("data/tmp"); a
dir <- paste("data/tmp/", a, sep = "")
n <- length(dir); n

for(j in 1:n){
  
  print(paste("PKU00",j,"：I am processing now!", sep = ""))
  
  collar_data <- read.csv(file = dir[j], header = T, sep = ",", stringsAsFactors = F)
  head(collar_data)
  
  collar_data <- collar_data[c("id","Date","Landcover")]
  head(collar_data);nrow(collar_data)
  
  mergetmp0 <- read.csv("data/length/PKU000.csv", 
                       sep=",", header=TRUE, stringsAsFactors = F) 
  mergetmp <- mergetmp0
  
  land_list <- c("bd","bw","crop","pl","psL","psH","water")
  
  for(i in 1:length(land_list)){
    print(land_list[i])

    tmp <- collar_data %>% subset(Landcover == land_list[i])
    
    if(nrow(tmp)==0){
      a <- mergetmp0
      colnames(a) <- c("label",land_list[i])
      a[2] <- NA
      mergetmp <- cbind(mergetmp,a[2])
    }else{
      freq.list <- as.data.frame(table(tmp$Date))
      colnames(freq.list) <- c("Date", "Freq")
      tmp1 <- left_join(mergetmp0,freq.list)
      colnames(tmp1) <- c("label", "Date", land_list[i])
      mergetmp <- cbind(mergetmp,tmp1[3])
    }
  }
  
  daily_lc0 <- as.data.frame(mergetmp)
  daily_lc0[is.na(daily_lc0)] <- 0
  daily_lc0$sum <- NA
  daily_lc0$sum <- apply(daily_lc0[,3:9], MARGIN = 1,sum)
  
  daily_lc <- daily_lc0[c("Date")]
  daily_lc$bd <- daily_lc0$bd/daily_lc0$sum
  daily_lc$bw <- daily_lc0$bw/daily_lc0$sum
  daily_lc$crop <- daily_lc0$crop/daily_lc0$sum
  daily_lc$pl <- daily_lc0$pl/daily_lc0$sum
  daily_lc$psL <- daily_lc0$psL/daily_lc0$sum
  daily_lc$psH <- daily_lc0$psH/daily_lc0$sum
  daily_lc$water <- daily_lc0$water/daily_lc0$sum

  dirPrint <- paste("data/DailyLandcover/PKU00", j, ".csv",sep = "")
  write.csv(daily_lc, file = dirPrint)
}
