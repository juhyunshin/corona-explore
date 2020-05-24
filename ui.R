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

dashboardPagePlus(
  header = dashboardHeaderPlus(
    enable_rightsidebar = FALSE,
    title = "Corona New Cases"
  ),
  sidebar = dashboardSidebar(
    selectizeInput("level","Level:",
                   c("National" = "st",
                     "State" = "nat")),
    conditionalPanel(
      condition = "input.level == 'nat'",
      uiOutput("state_input")
    ),
    numericInput("days","Number of days:",7,min = 1, max = 30),
    icon("bahai")
  ),
  body = dashboardBody(
    fluidRow(
      conditionalPanel(
        condition = "input.level == 'nat'",
        boxPlus(
          title = "County Level New Cases",
          width = 12,
          closable = FALSE,
          dataTableOutput("Nat"),
          footer = "Change = column 4 / column 3,
          Indicator Thresholds: More recoveries than new cases < 0,
          0 < Significant improvement <= 0.5,
          0.5 < Gradual Improvement <= 0.85,
          0.85 < No change <= 1.25,
          1.25 < Deterioration <= 2,
          Significant Deterioration > 2"
        ),
        boxPlus(
          title = "County Level New Cases",
          width = 12,
          closable = FALSE,
          plotlyOutput("Nat_chart")
        )
      ),
      conditionalPanel(
        condition = "input.level == 'st'",
        boxPlus(
          title = "State Level New Cases",
          width = 12,
          closable = FALSE,
          dataTableOutput("St"),
          footer = "Change = column 3 / column 2,
          Indicator Thresholds: More recoveries than new cases < 0,
          0 < Significant improvement <= 0.5,
          0.5 < Gradual Improvement <= 0.85,
          0.85 < No change <= 1.25,
          1.25 < Deterioration <= 2,
          Significant Deterioration > 2"
        ),
        boxPlus(
          title = "State Level New Cases",
          width = 12,
          closable = FALSE,
          plotlyOutput("St_chart")
        )
      )
      #last included date
    )
  )
)