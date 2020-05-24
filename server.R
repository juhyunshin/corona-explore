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

function(input,output){
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
  
  nat <- incr
  #Prep Data
  natldata <- reactive({
    #nat <- read.csv("National.csv")
    #days grouping
    varname1 <- paste0(input$days," Days before Last ",
                       input$days," Days")
    nat <- nat %>% 
      mutate(!!varname1 := rowSums(nat[,c((end-input$days*2+1):(end-input$days))]))
    #nat <- nat %>% group_by(Province_State) %>%
    #  mutate(State_Rank1 := min_rank(desc(!!as.name(varname1)))) %>%
    #  ungroup()
    #nat <- nat %>%
    #  mutate(Natl_Rank1 := min_rank(desc(!!as.name(varname1))))
    
    varname2 <- paste0("Last ",input$days," Days")
    nat <- nat %>% 
      mutate(!!varname2 := rowSums(nat[,c((end-input$days+1):end)]))
    #nat <- nat %>% group_by(Province_State) %>%
    #  mutate(State_Rank2 := min_rank(desc(!!as.name(varname2)))) %>%
    #  ungroup()
    #nat <- nat %>% 
    #  mutate(Natl_Rank2 := min_rank(desc(!!as.name(varname2))))
    
    nat <- nat %>% #mutate(State_Change = State_Rank1 - State_Rank2) %>%
      #mutate(Natl_Change = Natl_Rank1 - Natl_Rank2) %>%
      mutate(!!paste0("increase?") := 
               round((!!as.name(varname2)) / (!!as.name(varname1)),2)
      )
  })
  
  natldata2 <- reactive({
    #nat <- read.csv("National.csv")
    varname3 <- paste0(input$days2," Days before Last ",
                       input$days2," Days")
    nat <- nat %>% 
      mutate(!!varname3 := rowSums(nat[,c((end-input$days2*2+1):(end-input$days2))]))
    
    varname4 <- paste0("Last ",input$days2," Days")
    nat <- nat %>% 
      mutate(!!varname4 := rowSums(nat[,c((end-input$days2+1):end)]))
    
    nat <- nat %>% mutate(!!paste0("increase?") := 
                            round((!!as.name(varname4)) / (!!as.name(varname4)),2)
    )
  })
  
  #Renders:
  output$state_input <- renderUI({
    selectizeInput("state","State:",
                   c(levels(nat$Province_State)),
                   selected = "Oregon",
                   multiple = TRUE)
  })
  output$Nat <- renderDataTable({
    dat <- natldata() %>% select(Province_State,Admin2,tail(names(.),3)) %>%
      filter(Province_State %in% input$state)
  },
  options = list(searching = FALSE, pageLength = 15)
  )
  output$Nat2 <- renderDataTable({
    dat <- natldata2() %>% select(Province_State,Admin2,tail(names(.),3)) %>%
      filter(Province_State %in% input$state)
  },
  options = list(searching = FALSE, pageLength = 15)
  )
}