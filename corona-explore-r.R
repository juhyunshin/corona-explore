# Use csse_covid_19_daily_reports
# for county granularity of data
# cumulative data

# csse_covid_19_time_series
# time_series_covid19_confirmed_US
# has daily cumulative for each county

#admin2 = county for both

library(ggplot2)
library(ggthemes)
library(dplyr)
library(rlang)
library(chron)
library(scales)
library(hms)

setwd("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

date <- Sys.Date()

time_series <- read.csv("time_series_covid19_confirmed_US.csv")

# 11 identifiers before daily cumulative data starts

first = as.Date("2020-01-22")

start = 11
end = as.numeric(ncol(time_series))
varname <- as.Date(first+1)
ts <- time_series
y <- seq(1,end,1)

for(num in y) if (num <= end - start - 1){
  varname <- as.Date(first + num)
  ts <- ts %>% mutate(!!paste0(varname) := ts[,start+num+1] - ts[,start+num])
}
rm(num)
new_end = as.numeric(ncol(ts))
cols1 <- c((end+1):new_end)
cols2 <- c(1:(start+1))
cols <- c(cols2,cols1)
ts2 <- ts[,cols]

write.csv(ts2,file=paste0("C:/Users/juhyu/Downloads/corona_explore_",
                          date,".csv"), row.names = FALSE)
