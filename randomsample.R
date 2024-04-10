#install.packages("data.table")
library(data.table)

cmc0 <- fread("data/tmp/All_Growing.csv", sep=",", header=TRUE, stringsAsFactors = F)
cmc <- as(cmc0, "data.frame")
str(cmc)
nrow(cmc)

cmc_sample <- cmc[sample(nrow(cmc), 100000),]

#str(cmc_sample)
#nrow(cmc_sample)

fwrite(cmc_sample, file = "data/tmp/10w_All_G.csv")

