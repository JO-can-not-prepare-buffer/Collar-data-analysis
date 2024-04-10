#####
# function to rename columns
# kyq 202204
# Extract important data and Renaming columns 

#####
columns_renaming <- function(collar_data){

  wash_data <- with(collar_data, 
                    data.frame(id = 终端, lon = 经度, 
                               lat = 纬度, time = 时间,
                               elevation = 高度, activity = 运动量,
                               accuracy = 精度))

  return(wash_data)
}