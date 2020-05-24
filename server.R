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
library(plotly)

function(input,output){
  time_series <- read.csv("time_series_covid19_confirmed_US.csv")
  time_series <- time_series %>% 
    rename(State = Province_State, County = Admin2, County_State = Combined_Key)
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
  incr <- incr %>% arrange(incr$State)
  
  #state level
  state <- incr[,c(7,(start+1):end)]
  names <- c(colnames(state))
  names <- names[-1]
  state$State <- as.character(as.character(state$State))
  state <- state %>% group_by(State) %>%
    summarise_at(names,sum)
  state$State <- as.factor(as.factor(state$State))
  state <- as.data.frame(state)
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
    #nat <- nat %>% group_by(State) %>%
    #  mutate(State_Rank1 := min_rank(desc(!!as.name(varname1)))) %>%
    #  ungroup()
    #nat <- nat %>%
    #  mutate(Natl_Rank1 := min_rank(desc(!!as.name(varname1))))
    varname2 <- paste0("Last ",input$days," Days")
    nat <- nat %>% 
      mutate(!!varname2 := rowSums(nat[,c((end-input$days+1):end)]))
    #nat <- nat %>% group_by(State) %>%
    #  mutate(State_Rank2 := min_rank(desc(!!as.name(varname2)))) %>%
    #  ungroup()
    #nat <- nat %>% 
    #  mutate(Natl_Rank2 := min_rank(desc(!!as.name(varname2))))
    
    nat <- nat %>% #mutate(State_Change = State_Rank1 - State_Rank2) %>%
      #mutate(Natl_Change = Natl_Rank1 - Natl_Rank2) %>%
      mutate(Change := 
               round((!!as.name(varname2)) / (!!as.name(varname1)),2)
      )
    nat <- nat %>% mutate(Indicator = case_when(
    Change < 0 ~ "More recoveries than new cases",
    Change >= 0 & Change <= 0.5 ~ "Significant improvement",
    Change > 0.5 & Change <= 0.85 ~ "Gradual improvement",
    Change > 0.85 & Change <= 1.25 ~ "No change",
    Change > 1.25 & Change <= 2 ~ "Deterioration",
    Change > 2 ~ "Significant deterioration"
    ))
  })
  
  stdata <- reactive({
    varname1 <- paste0(input$days," Days before Last ",
                       input$days," Days")
    state <- state %>% 
      mutate(!!varname1 := rowSums(state[,c((end_st-input$days*2+1):(end_st-input$days))]))
    #state <- state %>% mutate(Rank1 := min_rank(desc(!!as.name(varname1))))
    varname2 <- paste0("Last ",input$days," Days")
    state <- state %>% 
      mutate(!!varname2 := rowSums(state[,c((end_st-input$days+1):end_st)]))
    #state <- state %>% mutate(Rank2 := min_rank(desc(!!as.name(varname2))))
    state <- state %>% #mutate(Rank_Change = Rank1 - Rank2) %>%
      mutate(Change :=
        round((!!as.name(varname2)) / (!!as.name(varname1)),2)
      )
    state <- state %>% mutate(Indicator = case_when(
      Change < 0 ~ "More recoveries than new cases",
      Change >= 0 & Change <= 0.5 ~ "Significant improvement",
      Change > 0.5 & Change <= 0.85 ~ "Gradual improvement",
      Change > 0.85 & Change <= 1.25 ~ "No change",
      Change > 1.25 & Change <= 2 ~ "Deterioration",
      Change > 2 ~ "Significant deterioration"
    ))
  })
  
  #Renders:

  output$state_input <- renderUI({
    selectizeInput("state","State:",
                   c(levels(nat$State)),
                   selected = "Oregon",
                   multiple = TRUE)
  })
  output$Nat <- renderDataTable({
    dat <- natldata() %>% select(State,County,tail(names(.),4)) %>%
      filter(State %in% input$state)
  },
  options = list(searching = FALSE, pageLength = 15)
  )
  output$Nat_chart <- renderPlotly({
    dat <- natldata() %>% select(State,County,tail(names(.),4)) %>%
      filter(State %in% input$state)
    
    dat <- dat %>% arrange(desc(dat[,4]))
    dat <- dat[,c(1:4)]
    dat2 <- head(dat,15)
    dat2 <- dat2 %>% arrange(dat2[,4])
    
    nchart <- plot_ly(dat2)
    nchart <- nchart %>% 
      add_trace(dat2, x = dat2[,3], y = dat2$County, 
                yaxis = list(type = 'category'), type = 'bar',
                name = paste0(input$days," Days before Last ",
                              input$days," Days")) %>%
      add_trace(dat2, x = dat2[,4], y = dat2$County,
                yaxis = list(type = 'category'), type = 'bar',
                name = paste0("Last ",input$days," Days"))

  })
  
  output$St <- renderDataTable({
    dat <- stdata() %>% select(State, tail(names(.),4))
  })
  output$St_chart <- renderPlotly({
    dat <- stdata() %>% select(State, tail(names(.),4))
    
    dat <- dat %>% arrange(desc(dat[,3]))
    dat <- dat[,c(1:3)]
    dat2 <- head(dat,15)
    dat2 <- dat2 %>% arrange(dat2[,3])
    
    schart <- plot_ly(dat2)
    schart <- schart %>% 
      add_trace(dat2, x = dat2[,2], y = dat2$State,
                yaxis = list(type = 'category'), type = 'bar',
                name = paste0(input$days," Days before Last ",
                              input$days," Days")) %>%
      add_trace(dat2, x = dat2[,3], y = dat2$State,
                yaxis = list(type = 'category'), type = 'bar',
                name = paste0("Last ",input$days," Days"))
  })
}