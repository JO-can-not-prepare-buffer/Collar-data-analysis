#daily length of movement
#data: 3h interval collar data
#kyq202303

library(dplyr)
library(ggplot2)

a <- list.files("data/raw"); a
dir <- paste("data/raw/", a, sep = "")
n <- length(dir); n

for(j in 1:n){

  print(paste("PKU00",j,"：I am processing now!", sep = ""))
  
  collar_data <- read.csv(file = dir[j], header = T, sep = ",", stringsAsFactors = F)
  head(collar_data)
  
  collar_data$sl_3h_right <- NA
  
  for(i in 1:nrow(collar_data)){
    collar_data$sl_3h_right[i] <- 
      collar_data$sl_3h[i] * ( 3 * 2 * pi / 24 )
  }
  
  length.list <- collar_data %>% group_by(Date) %>% 
    summarise(mean(sl)*8, )
  head(length.list)
  
  freq.list <- as.data.frame(table(collar_data$Date))

  dirPrint <- paste("data/length/PKU00", j, ".csv",sep = "")
  #write.csv(length.list, file = dirPrint)
  
  df1 <- read.csv("data/length/PKU000.csv", 
                  sep=",", header=TRUE, stringsAsFactors = F) 
  df2 <- cbind(length.list,freq.list$Freq);head(df2)
  
  colnames(df1) <- c("label","Date")
  df1 <- tibble::as_tibble(df1);head(df1)
  
  colnames(df2) <- c("Date", paste("PKU00", j, sep = ""), "Freq")
  df2 <- tibble::as_tibble(df2);head(df2)
  

  df3 <- left_join(df1, df2)
  df3 <- select(df3, 2:4);head(df3)
  
  dirPrint1 <- paste("data/length/datePKU00", j, ".csv",sep = "")
  write.csv(df3, file = dirPrint1)
  
  # Plot----
  # ml <- df3;head(ml)
  # colnames(ml) <- c("Date", "sl")
  # ml$Date <- factor(ml$Date)
  # ggplot(ml, aes(x = Date, y = sl, group = 1)) + 
  #   geom_line(linetype = "dotted") + 
  #   labs(title = paste("PKU00", j, sep = ""))
}
