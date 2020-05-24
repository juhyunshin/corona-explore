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

dashboardPagePlus(
  titlePanel("Corona New Cases"),
  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE
  ),
  sidebar = dashboardSidebar(
    selectizeInput("level","Level:",
                   c("National" = "nat",
                     "State" = "st")),
    uiOutput("state_input"),
    numericInput("days","Number of days1:",7,min = 1, max = 30),
    numericInput("days2", "Number of days2:",14,min = 1, max = 30)
  ),
  body = dashboardBody(
    fluidRow(
      conditionalPanel(
        condition = "input.level == 'nat'",
        boxPlus(
          title = "County Level New Cases - Table 1",
          width = 6,
          closable = FALSE,
          dataTableOutput("Nat")
        ),
        boxPlus(
          title = "County Level New Cases - Table 2",
          width = 6,
          closable = FALSE,
          dataTableOutput("Nat2")
        )
      ),
      conditionalPanel(
        condition = "input.level == 'st'",
        boxPlus(
          title = "State Level New Cases",
          width = 12,
          closable = FALSE,
          dataTableOutput("St")
        )
      )
    )
  )
)