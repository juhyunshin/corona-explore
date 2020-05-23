library(ggplot2)
library(ggthemes)
library(dplyr)
library(rlang)
library(chron)
library(scales)
library(hms)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(DT)


# does confirmed file include recovered? looks to be negative incremental so i think yes
setwd("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/COVID-19/csse_covid_19_data/csse_covid_19_time_series")
time_series <- read.csv("time_series_covid19_confirmed_US.csv")

first = as.Date("2020-01-22")

# 11 identifiers before daily cumulative data starts

start = 11
end = as.numeric(ncol(time_series))
y <- seq(1,end,1)

for(num in y) if (num <= end - start - 1){
  varname <- as.Date(first + num)
  time_series <- time_series %>% 
    mutate(!!paste0(varname) := time_series[,start+num+1] - time_series[,start+num])
}
rm(num)

cols <- c(1:(start+1),(end+1):as.numeric(ncol(time_series)))

#incremental data table
incr <- time_series[,cols]
incr <- incr %>% arrange(incr$Province_State)

#state level
state <- incr[,c(7,(start+1):end)]
names <- c(colnames(state))
names <- names[-1]
state$Province_State <- as.character(as.character(state$Province_State))
state <- state %>% group_by(Province_State) %>%
  summarise_at(names,sum)
end_st = as.numeric(ncol(state))


#write to RDS file
setwd("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/corona-explore")
saveRDS(incr,"National.RDS")
saveRDS(state,"State.RDS")
write.csv(incr,file=paste0("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/corona-explore",
                           "/National.csv"), row.names = FALSE)
write.csv(state,file=paste0("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/corona-explore",
                             "/State.csv"), row.names = FALSE)
