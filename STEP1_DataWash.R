#pls firstly run: "delete_wrong" "day_night" "columns_reaming"

#### Preamble ####

# Install & Libraries #
# pcks <- c("knitr", "lubridate", "maptools", "raster", "move", "amt", 
#           "ggmap", "tibble", "leaflet","dplyr")
# sapply(pcks, install.packages)
#library(lubridate)
#library(maptools)
#library(raster)
#library(tibble)
#library(leaflet)
#library(openxlsx)
library(knitr)
library(move)
library(amt) 
library(ggmap)
library(dplyr)
options(width=165,digits.secs = 3)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = F)

# Record time for running all code #
ptm<-proc.time()

# Set the seed for the random number generator #
# so it will be possible to reproduce the random points #
set.seed(10299)


# Get CMC data #
cmc.move <- read.csv("data/PKU011.csv", sep=",", 
                     header=TRUE, stringsAsFactors = F)
str(cmc.move)

# Create a data frame #
cmc.dat0 <- as(cmc.move, "data.frame")
str(cmc.dat0)

#### Data cleaning ####

# Extract important data and Renaming columns #
cmc.dat <- columns_renaming(cmc.dat0)
head(cmc.dat)
#write.csv(cmc.dat, file = "tmp/wash/PKU001_col_rename.csv")
head(cmc.dat$time)

# Delete observations where missing lat or lon or a time #
ind<-complete.cases(cmc.dat[,c("lat", "lon", "time")])
cmc.dat<-cmc.dat[ind==TRUE,]

# Check for duplicated observations #
ind2<-cmc.dat %>% select(time, lon, lat, id) %>%
  duplicated
sum(ind2)
cmc.dat<-cmc.dat[ind2!=TRUE,]

# Make "time" a date/time variable #
cmc.dat$time<-as.POSIXct(cmc.dat$time, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
head(cmc.dat$time)


# Duration #
a <- min(cmc.dat$time)
b <- max(cmc.dat$time)
duration = as.Date(b) - as.Date(a) + 1
b
duration

# Effective working day #
c <- as.Date(cmc.dat$time)
d <- table(c)
nrow(d)
#str(d)


# Map briefly #
ggplot(cmc.dat, aes(x = lon, y = lat, col = id)) + geom_path() + 
  labs(title = "All points WGS1984")


#### Projections ####

# into UTM #
# install.packages("PBSmapping")
library(PBSmapping)
require(PBSmapping)
ll <- with(cmc.dat, data.frame(X = lon, Y = lat))
attributes(ll)$projection <- "LL"
xy.km <- convUL(ll, km=TRUE)
str(xy.km)
diff(range(xy.km$X))
diff(range(xy.km$Y))

# Add these transformed coordinates to the cmc data frame #
cmc.dat_UTM <- data.frame(cmc.dat, x=xy.km$X*1000, y=xy.km$Y*1000)
head(cmc.dat_UTM)
#ggplot(cmc.dat_UTM, aes(x = x, y = y, col = id)) + geom_path()



#### Creating a track in amt ####

trk <- mk_track(cmc.dat, .x=lon, .y=lat, .t=time, id = id, activity = activity, 
                crs = 4326)

# day or night # 
## use average Lon & Lat in Menyuan
trk1 <- day_night(trk, 101.5, 37.5)
trk <- trk1
head(trk)
trk <- transform_coords(trk, crs_to = 32647)

trk.class<-class(trk)
trk.class


#### Movement Characteristics ####

# if more than one cats #
# create a nested object by individual #
nesttrk<-trk%>%nest(data = -id)
nesttrk

# add a columns to each nested column of data using purrr::map #
require(purrr)

trk<-trk %>% nest(data = -id) %>% 
  mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd))%>%unnest(., cols = c(data, dir_abs, dir_rel, sl, nsd_))

class(trk)<-trk.class

# calculate month, year, hour, week of each observation and #
# append these to the dataset Unlike the movement charactersitics #
# trk<-trk%>% 
#   mutate(
#     week=week(t_),
#     month = month(t_, label=TRUE), 
#     year=year(t_),
#     hour = hour(t_)
#   )
class(trk)
class(trk)<-trk.class
str(trk)

trk

trklonlat <- trk1[,c(1,2)]
colnames(trklonlat) <- c('lon', 'lat')
trknew <- cbind(trk,trklonlat)

cmc_trk.df <- as(trknew, "data.frame")
str(cmc_trk.df)
#write.csv(cmc_trk.df, file = "data/wash/PKU002trk.csv")


#### Delete wrong point ####
n_origin <- nrow(cmc_trk.df)
n_origin

cmc_washed2 <- delete_wrong(cmc_trk.df)
head(cmc_washed2$deleteRow)

wash_remain <- subset(cmc_washed2, cmc_washed2$deleteRow != "1")
n_ramain <- nrow(wash_remain)
n_ramain

wash_delete <- subset(cmc_washed2, cmc_washed2$deleteRow == "1")
n_delete <- nrow(wash_delete)
n_delete

# Map briefly #
ggplot(cmc_washed2, aes(x = x_, y = y_, col = id)) + geom_path() + 
  labs(title = "All points")

ggplot(wash_remain, aes(x = x_, y = y_, col = id)) + geom_path() + 
  labs(title = "remain points")

ggplot(wash_delete, aes(x = x_, y = y_, col = id)) + geom_path() + 
  labs(title = "delete points")


# output
write.csv(wash_remain, file = "data/PKU011_new.csv")
#write.csv(wash_delete, file = "data/wash/PKU001delete.csv")
#write.csv(cmc_washed2, file = "data/wash/PKU001mark.csv")

