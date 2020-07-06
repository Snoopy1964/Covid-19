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
library("ggpubr")
library("ggrepel")
# library("geojsonio")
library("wppExplorer")

#-----------------------------------------------------
# initialization
#-----------------------------------------------------
cat("---> Initialization Ui.R\n")


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

debug.on   <- options("Debug.Dashboard")[[1]]
force.load <- options("Force.Load")[[1]]

if(debug.on) {cat("------> load country data from ")}
if(force.load) {
  if(debug.on) {cat("web\n")}
  countries <<- loadCountries()
  save(countries, file = "data//countries.rda")
  if(debug.on) {cat("------> country data saved\n")}
} else {
  load("data//countries.rda", .GlobalEnv)
}

map.country.charcode <- function(ccc) {
  return(
    (countries %>%
       dplyr::filter(charcode %in% ccc) %>%
       select(country.iso))[[1]]
  )
}

#-----------------------------------------------------
# setup global variables
#-----------------------------------------------------
coi.id <- c("BE", "BR", "CA", "DK", "FI", "FR","DE", "GR", "IS", "IN", "IL", "IT", "JP", "LU", "MX", "NL", "NO", "PL", "PT", "RU", "ES", "SE", "CH", "TR", "GB", "US")
coi <- map.country.charcode(coi.id)

country.list.inputs <- list(
  "World" = 1,
  "> 2 M population" = 2,
  "Europe" = 3,
  "Top 20" = 4,
  "Snoopy's List" = 99
)

region.list.inputs <- list(
  "Country" = 0,
  "Region" = 1,
  "Continent" = 2,
  "Major GEO3 regions" = 3,
  "GEO3 regions" = 4
)

#--------------------
# header of dashboard
#--------------------

header  <- dashboardHeader(title = "Snoopy's Covid 19 dashboard")

#--------------------
# sidebar of dashboard
#--------------------

sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("World Overview"   , tabName = "world_dashboard", icon = icon("dashboard")),
    menuItem("Statistics (day)", tabName = "statistics"    , icon = icon("th")),
    menuItem("Countries", tabName = "country_dashboard", icon =icon("dashboard"),
             menuSubItem("Details of Country", tabName = "country_details"),
             selectInput("select", label = FALSE, 
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                         selected = 1)),
    menuItem(
      "Charts", tabName = "charts", icon=icon("bar-chart-o"), startExpand = TRUE,
      menuSubItem("Cases, Incidences & Deaths", tabName = "compareCountries"),
      menuSubItem("Compare Countries",          tabName = "compare", icon = icon("th"))
    ),
    dateInput("date.snapshot", 
              tagList(
                h4("Date input"),
                div("Data will be updated at JHU around midnighth. Latest available data are from the previous day.",
                       style = "white-space: normal; font-size: xx-small")
              ),
              value = today()-1),
    radioButtons("region.select", h4("Aggregation Level"), choices = region.list.inputs, selected = 4),
    # radioButtons("country.selection", h4("Select Countries"), choices = country.list.inputs, selected = 1),
    actionButton("load.data", "Reload data", icon = icon("refresh"))
  )
)
#--------------------
# body of dashboard
#--------------------

body <- dashboardBody(
  tabItems(
    # Tab: world data
    tabItem(
      tabName = "world_dashboard",

      fluidRow(
        valueBoxOutput("world.total.cases", width=3),
        valueBoxOutput("world.active.cases", width=3),
        valueBoxOutput("world.recovered.cases", width=3),
        valueBoxOutput("world.death", width=3)
      ),

      fluidRow(
        tabBox(
          id = "tabBox.world",
          side = "left",
          width = 12,
          selected = "Cases since Feb. 2020",
          tabPanel("Cases since Feb. 2020",            plotOutput("summary.charts.world",    height = 750)),
          tabPanel("Top regions at Selected Day",      plotOutput("summary.charts.regions", height = 750)),
          tabPanel("Top 20 Countries at Selected Day", plotOutput("summary.charts.countries.top20", height = 750))
        ),
      ),
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
      tabName = "country_dashboard",
      column(
        width = 3,
        DT::dataTableOutput("cases.countries1")
      ),
      column(
        width = 9,
        DT::dataTableOutput("cases.countries2")
      )
    ),
    tabItem(
      tabName = "compareCountries",
      titlePanel("Compare countries"),
      # Boxes need to be put in a row (or column)
      fluidRow(
        column(
          width  = 12,
          height = 200,
          # width  = 500,
          box(
            # title = "Select countries to compare",
            selectInput("countryId" , 
                        "Select countries to compare", coi, 
                        selected = c("Germany", 
                                     "United States",
                                     "Russian Federation",
                                     "United Kingdom"), 
                        multiple = TRUE, 
                        width = "auto")
          ),
          box(
            # title = "Input Data",
            radioButtons(
              "data.selected",
              "Input Data", c("normalized cases per 100.000 residents", "absolute cases"),
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
          sliderInput("date.range", "date range",value = as.Date("2020-03-01"), min=as.Date("2020-01-22"), max=today()-1)
        ),
        box(
          width = 9,
          plotOutput("compare.countries.active", 
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
