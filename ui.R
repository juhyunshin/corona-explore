library(ggplot2)
library(ggthemes)
library(dplyr)
library(rlang)
library(chron)
library(scales)
library(hms)
library(crayon)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(DT)
library(plotly)

dashboardPagePlus(
  skin = "black",
  header = dashboardHeaderPlus(
    enable_rightsidebar = FALSE,
    titleWidth = 225,
    title = "Corona New Cases"
  ),
  sidebar = dashboardSidebar(
    tags$head(tags$style(HTML('
        {
          background-color: #212223;
        }
      '))),
    selectizeInput("level","Level:",
                   c("National" = "st",
                     "State" = "nat")),
    conditionalPanel(
      condition = "input.level == 'nat'",
      uiOutput("state_input")
    ),
    numericInput("days","Choose grouping in days:",7,min = 0, max = 31),
    icon("bahai"),
    width = 225
  ),
  body = dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-sidebar .sidebar .sidebar-menu {
          background-color: #212223;
        }
      '))),
    fluidRow(
      column(12,
             uiOutput("text"),
             p("Created by ",a("Juhyun Shin",href="https://github.com/juhyunshin/corona-explore"),
             " using data from the COVID-19 Data Repository by the CSSE at", 
             a("Johns Hopkins University",href="https://coronavirus.jhu.edu/map.html"),br()),
             h6("This Shiny App was created to provide a quick snapshot of emerging cases
             as states begin reopening.",br(),"Use ",tags$strong("level")," in the left hand
             sidebar to view at the ",tags$strong("state")," or ",tags$strong("county"),
             "level.",br(),"Choose ",tags$strong("number of days")," in the left hand
             sidebar to change the ",tags$strong("lookback")," period.",br(),
             "Click on the ",tags$strong("column names")," in the data table to ",
             tags$strong("re-sort"),"the data.")),
      conditionalPanel(
        ###County Level###
        condition = "input.level == 'nat'",
        boxPlus(
          title = "National Level New Cases",
          width = 12,
          closable = FALSE,
          tableOutput("All")
        ),
        boxPlus(
          title = "County Level New Cases",
          width = 6,
          closable = FALSE,
          dataTableOutput("Nat")
        ),
        boxPlus(
          title = "Top 15 Counties with most New Cases",
          width = 6,
          closable = FALSE,
          plotlyOutput("Nat_chart")
        )
      ),
      conditionalPanel(
        ###State Level###
        condition = "input.level == 'st'",
        boxPlus(
          title = "National Level New Cases",
          width = 12,
          closable = FALSE,
          tableOutput("All2")
        ),
        boxPlus(
          title = "State Level New Cases",
          width = 6,
          closable = FALSE,
          dataTableOutput("St")
        ),
        boxPlus(
          title = "Top 15 States with most New Cases",
          width = 6,
          closable = FALSE,
          plotlyOutput("St_chart")
        )
      ),
      column(12,
             uiOutput("text2"),
             h5("Indicator Thresholds:",br(),
                "More recoveries than new cases < 0,",br(),
                "0 < Significant improvement <= 0.5,",br(),
                "0.5 < Gradual Improvement <= 0.85,",br(),
                "0.85 < No change <= 1.25,",br(),
                "1.25 < Deterioration <= 2,",br(),
                "Significant Deterioration > 2"),
             )
      #last included date
    )
  )
)