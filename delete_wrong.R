#####
# function to Delete Wrong GPS Data
# kyq 202205
## Calculate steplength in standard time unit (20 min). Choose standard data.
## which to delete: Both itself and the previous one have large steplenth.
## mark these data with "deleteRow == 1"

#####
delete_wrong <- function(cmc){
  
  library(dplyr)
 
  cmc$time_interval <- NA
  cmc$sl_20min <- 0
  
  cmc$mark <- NA
  cmc$breakpoint <- NA
  cmc$max0.05 <-NA
  cmc$deleteRow <-NA
  
  interval_20min <- (20 / 60) * 2 * pi / 24
  interval_3h <- (180 / 60) * 2 * pi / 24
  
  #### Calculate step lentgh in a unit time 20 min ####
  for(i in 1:(nrow(cmc)-1)){
    date0 <- as.Date(cmc$Date[i])
    date1 <- as.Date(cmc$Date[i+1])
    time0 <- cmc$RTime[i]
    time1 <- cmc$RTime[i+1]
    cmc$time_interval[i] <- (date1 - date0) * 2 * pi + (time1 - time0)
    
    if(cmc$time_interval[i] > interval_3h){
      cmc$breakpoint[i] <- "BreakStart"
      cmc$breakpoint[i+1] <- "BreakEnd"
      cmc$sl_20min[i] <- 0
      cmc$mark[i] <- 2
    }
    else if(cmc$time_interval[i] < (interval_20min*2)){
      cmc$sl_20min[i] <- cmc$sl[i]
      cmc$mark[i] <- 0 ## this will be used in iSSF after
    }
    else{
      cmc$sl_20min[i] <- cmc$sl[i]/cmc$time_interval[i]
      cmc$mark[i] <- 1
    }
  }  
  cmc$sl_20min[nrow(cmc)] <- 0
  head(cmc)
  
  #### Sort and get max 5% value ####
  newdata <- subset(cmc$sl_20min, cmc$sl_20min > 0)
  sortdata <- sort(newdata, decreasing = TRUE)
  length(sortdata)
  x <- ceiling(length(sortdata)*0.05)
  xx <- sortdata[x]
  xx
  
  #### Mark 5% ####
  for(i in 1:(nrow(cmc)-1)){
    if(cmc$sl_20min[i] > xx){
      cmc$max0.05[i] <- 1
    }else{
      cmc$max0.05[i] <- 0
    }
  }
  
  #### delete ####
  for(i in 2:(nrow(cmc)-1)){
    if((cmc$max0.05[i] == 1)&(cmc$max0.05[i-1] == 1)){
      cmc$deleteRow[i] <- 1
    }else{
      cmc$deleteRow[i] <- 0
    }
  }
  
  return(cmc)
}