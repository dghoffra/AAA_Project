# R - Script #
# Clean global Environment
rm(list=ls())

# Install the required package with:
# install.packages("RSocrata")
library("RSocrata")

library(lubridate)
library(zoo)
library(tidyverse)

my_app_token = "nCdh8C1YzOWnMlVPfWEqqzNLF"
my_email     = "mindl@wiso.uni-koeln.de"
my_password  = "AAA_Class_2021"


# example Api call: https://data.cityofchicago.org/resource/wrvz-psew.json?trip_start_timestamp=2017-10-12T17:15:00.000


# Make list of date times in 15minute breaks for download
dates = tibble(time_d = seq(as_datetime("2013-01-01T12:00:00.000"), # from first day data was recorded
                            now()-1296000, # to until today minis 15 days, as data is not live.
                            by= 900))
dates$Year_month <- as.yearmon(dates$time_d)

# converting dates to the format needed for API call
dates <- dates %>% mutate(time_d = paste0(gsub(pattern = " ",replacement = "T", substr(as.character(time_d),1,19)),".000"))


# get indeces of each month for loop call
obs_month <- as.data.frame(table("Year_mon" = dates$Year_month))
obs_month <- obs_month %>% mutate(end_index = cumsum(obs_month$Freq),
                                  start_index = lag(end_index))
obs_month$start_index[1] <- 1

# looping through all month available
# if only specific month is needed. comment out this outer loop and only run the inner one with the specific month.
for(j in 1:nrow(obs_month)){
 
# with the outer loop, it loops through the entire time frame avaialbe (2013 till 2021) and saves the data monthwise
start_month <- obs_month$Year_mon[j]
end_month <-  obs_month$Year_mon[j]

df = tibble()

for(i in obs_month$start_index[obs_month$Year_mon==start_month]:obs_month$end_index[obs_month$Year_mon==end_month]){
  print(paste(i,obs_month$end_index[obs_month$Year_mon==end_month], sep = "/"))
  temp <- read.socrata(
    paste0("https://data.cityofchicago.org/resource/wrvz-psew.json?trip_start_timestamp=", dates$time_d[i]),
    app_token = my_app_token,
    email     = my_email,
    password  = my_password
  )
  df <- bind_rows(df,temp %>% select(!c("pickup_centroid_location.type",        
                                        "pickup_centroid_location.coordinates",
                                        "dropoff_centroid_location.type",
                                        "dropoff_centroid_location.coordinates"
                                        )))
  }

  # adjust file path, for file destination
con<-file(paste0("C:/Users/Felix/Documents/AAA/Data/taxi_trips_", start_month, ".csv"),encoding="UTF-8")
write.table(df, file=con, row.names=F, sep = ",")

  # clear ram
rm(df)
gc()

}
