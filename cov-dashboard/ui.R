#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(DT)
library("utils")
library("httr")
library("jsonlite")

library("lubridate")
library("tidyverse")
library("readxl")
library("ggmap")
# library("ggplotgui"
library("ggrepel")
# library("geojsonio")
library("wppExplorer")

#-----------------------------------------------------
# initialization
#-----------------------------------------------------
cat("---------------------> Initialization Ui.R\n")
print(environment())
print(getwd())


# load API key
if (file.exists(".api_key.R")) {
  source(".api_key.R")
  message(cat("API key used: ",x.rapidapi.key))
} else {
  warning("no file .api_key.R found! data updates are not possible!", immediate. = TRUE)
  
}

#-----------------------------------------------------
# some helper functions
#-----------------------------------------------------
source("01_helper_func_data.R")

load("data//countries.rda", .GlobalEnv)

map.country.charcode <- function(ccc) {
  return(
    (countries %>%
       dplyr::filter(charcode %in% ccc) %>%
       select(country.iso))[[1]]
  )
}




#df <<- dataInput()

#-----------------------------------------------------
# setup global variables
#-----------------------------------------------------
coi.id <- c("BE", "BR", "CA", "DK", "FI", "FR","DE", "GR", "IS", "IN", "IL", "IT", "JP", "LU", "MX", "NL", "NO", "PL", "PT", "RU", "ES", "SE", "CH", "TR", "GB", "US")
coi <- map.country.charcode(coi.id)

#--------------------
# header of dashboard
#--------------------

header  <- dashboardHeader(title = "Snoopy's Covid 19 dashboard")

#--------------------
# sidebar of dashboard
#--------------------

sidebar <-dashboardSidebar(
    sidebarMenu(
      menuItem("World Overview"   , tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Statistics (day)", tabName = "statistics"    , icon = icon("th")),
      menuItem("Active Cases", tabName = "cases"    , icon = icon("th")),
      actionButton("load.data", "Reload data", icon = icon("refresh")),
      dateInput("date.snapshot", h3("Date input"), value = today())
      #dateInput("date.snapshot", h3("Date input"), value = as.Date("2020-05-30"))
    )
  )
#--------------------
# body of dashboard
#--------------------

body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("world.total.cases", width=3),
          valueBoxOutput("world.active.cases", width=3),
          valueBoxOutput("world.recovered.cases", width=3),
          valueBoxOutput("world.death", width=3)
        ),
        fluidRow(
          box(
            plotOutput("world.active")
          ),
          box(
            plotOutput("world.incidents")
          )
        ),
        fluidRow(
          box(
            plotOutput("countries.active")
          ),
          box(
            plotOutput("countries.incidents")
          )
        )
      ),
      # First tab content
      tabItem(
        tabName = "statistics",
        titlePanel("Numbers per day"),
        fluidRow(
          DT::dataTableOutput("cases.countries")
        )
      ),
      tabItem(
        tabName = "cases",
        titlePanel("Compare countries"),
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(
            width  = 12,
            height = 200,
            # width  = 500,
            box(
              title = "Select countries to compare",
              selectInput("countryId" , 
                          NA, coi, 
                          selected = c("Germany", 
                                       "United States",
                                       "Russian Federation",
                                       "United Kingdom"), 
                          multiple = TRUE, 
                          width = "auto")
            ),
            box(
              title = "Input Data",
              radioButtons(
                "data.selected",
                NA, c("normalized cases per 100.000 residents", "absolute cases"),
                selected = "normalized cases per 100.000 residents")
            )
          )
        ),
        fluidRow(
          # column(
          #   width  = 12,
          #   height = 500,
          box(
            width = 9,
            plotOutput("active", 
                       height = "800px", 
                       click  = clickOpts("active_click"),
                       hover  = hoverOpts("active_hover", delay = 100, delayType = "debounce")
            ),
            uiOutput("hover_info")
          ),
          box(
            width = 3,
            verbatimTextOutput("click_info")
            # verbatimTextOutput("hover_info")
            #   )
          )
        )
      )
    )
  )

#--------------------
# put UI together 
#--------------------
ui <- dashboardPage(header, sidebar, body)
