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

# cd COVID-19
# git pull origin master

setwd("C:/Users/juhyu/OneDrive/Documents/Documents/udacity-git-course/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

date <- Sys.Date()

# does confirmed file include recovered? looks to be negative incremental so i think yes
time_series <- read.csv("time_series_covid19_confirmed_US.csv")

first = as.Date("2020-01-22")

# 11 identifiers before daily cumulative data starts

start = 11
end = as.numeric(ncol(time_series))

ts <- time_series
y <- seq(1,end,1)

for(num in y) if (num <= end - start - 1){
  varname <- as.Date(first + num)
  ts <- ts %>% mutate(!!paste0(varname) := ts[,start+num+1] - ts[,start+num])
}
rm(num)
new_end = as.numeric(ncol(ts))
# +1 to include first date where cumulative and incremental is the same
cols1 <- c(1:(start+1))
cols2 <- c((end+1):new_end)
cols <- c(cols1,cols2)
rm(cols1)
rm(cols2)
# summarise at county level
incr <- ts[,cols]
incr <- incr %>% arrange(incr$Province_State)

scols <- c(7,(start+1):end)
state <- incr[,scols]
state <- state %>% arrange(state$Province_State)
names <- c(colnames(state))
names <- names[-1]
state$Province_State <- as.character(as.character(state$Province_State))

#summarise at state level
state2 <- state %>% group_by(Province_State)
state2 <- state2 %>% summarise_at(names,sum)

#last 7 day sums
varname1 <- paste0("7","_Days_before_Past_","7","_Days")
incr <- incr %>% mutate(!!varname1 := rowSums(incr[,c((end-7*2+1):(end-7))]))
incr <- incr %>% group_by(Province_State) %>%
  mutate(St_Rank1 := min_rank(desc(!!as.name(varname1)))) %>%
  ungroup()
incr <- incr %>% mutate(Natl_Rank1 := min_rank(desc(!!as.name(varname1))))
varname2 <- paste0("Past_","7","_Days")
incr <- incr %>% mutate(!!varname2 := rowSums(incr[,c((end-7+1):end)]))
incr <- incr %>% group_by(Province_State) %>%
  mutate(St_Rank2 := min_rank(desc(!!as.name(varname2)))) %>%
  ungroup()
incr <- incr %>% mutate(Natl_Rank2 := min_rank(desc(!!as.name(varname2))))
incr <- incr %>% mutate(St_Change = St_Rank1 - St_Rank2) %>%
  mutate(Natl_Change = Natl_Rank1 - Natl_Rank2) %>%
  mutate(increase = case_when(
    (!!as.name(varname2)) > (!!as.name(varname1)) ~ "Yes",
    TRUE ~ "No"
  ))

state_end = as.numeric(ncol(state2))
state2 <- state2 %>% 
  mutate(!!varname1 := rowSums(state2[,c((state_end-7*2+1):(state_end-7))]))
state2 <- state2 %>% mutate(Rank1 := min_rank(desc(!!as.name(varname1))))
state2 <- state2 %>% 
  mutate(!!varname2 := rowSums(state2[,c((state_end-7+1):state_end)]))
state2 <- state2 %>% mutate(Rank2 := min_rank(desc(!!as.name(varname2))))
state2 <- state2 %>% mutate(Rank_Change = Rank1 - Rank2) %>%
  mutate(increase = case_when(
    (!!as.name(varname2)) > (!!as.name(varname1)) ~ "Yes",
    TRUE ~ "No"
  ))


write.csv(incr,file=paste0("C:/Users/juhyu/Downloads/corona_all_incremental_",
                          date,".csv"), row.names = FALSE)
write.csv(state2,file=paste0("C:/Users/juhyu/Downloads/corona_state_incremental_",
                           date,".csv"), row.names = FALSE)
