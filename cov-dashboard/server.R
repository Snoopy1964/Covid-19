#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("utils")
library("httr")
library("jsonlite")

library("lubridate")
library("tidyverse")
library("readxl")
library("ggmap")
# library("ggplotgui"
library("ggrepel")
library("geojsonio")
library("wppExplorer")
library(shiny)
library(shinydashboard)

#-----------------------------------------------------
# initialization
#-----------------------------------------------------
cat("---------------------> Initialization Server.R\n")
print(environment())
print(getwd())


# load API key
if (file.exists(".api_key1.R")) {
  source(".api_key.R")
  message(cat("API key used: ",x.rapidapi.key))
} else {
  warning("no file .api_key.R found! data updates are not possible!", immediate. = TRUE)
  
}


#-----------------------------------------------------
# some helper functions
#-----------------------------------------------------

source("01_helper_func_data.R")
# 
# 
#-----------------------------------------------------
# setup global variables
#-----------------------------------------------------
countries <- loadCountries()

cat("\n---------------------> Server.R: countries\n")
print(countries)
cat("\n---------------------> Server.R: countries end\n")

# dataInput <- function() {
#   if( exists("df")) return(df)
#   if(!exists("df")){ 
#     df <- loadData()
#     return(df)
#   }
# }
print(getwd())
load("data//cases.rda", .GlobalEnv)
print(df)
print( parent.frame())

# Define server logic required to draw a histogram
server <- function(input, output) {
  # df <- load("data//cases.rda")
  cat("---------------------> server()\n")
  load.data <- eventReactive(input$load.data, {
    cat("hallo world!")
    df
  })
  
  # source("10_server_data.R")
  #--------------------------------------------------------------
  # definition of data functions to be used inside server only!
  #--------------------------------------------------------------
  df.active <- reactive({
    return(df %>% 
             dplyr::filter(country.iso %in% 
                             input$countryId)
           )
  })
  df.world <- reactive({
    return(df              %>% 
             group_by(day) %>% 
             summarize(cases.total  = sum(cases.total),
                       cases.active = sum(cases.active + deaths.total),
                       deaths.total = sum(deaths.total),
                       population   = sum(population, na.rm = TRUE))
    )
  })
  df.latest <- reactive({
    return( df %>% group_by(charcode)  %>% dplyr::filter(day == input$date.snapshot))
  })
  
  world.cases <- function(sum.attribute, date = input$date.snapshot) {
    dummy <- input$load.data
    
    return(
      df %>% 
        group_by(day) %>% 
        select(c("day", sum.attribute)) %>%
        rename(cases = sum.attribute)   %>% 
        summarize(sum(cases))           %>%
        dplyr::filter(day == date)
    )
  }
  
  
  
  # source("20_server_output.R")
  #----------------------------------------------------------
  # generate plotes to be used inside server output
  #----------------------------------------------------------
  
  generateActive <- function() {
  # temporary!!!!!!
    ncol <- 3
    
    if(input$data.selected == "absolute cases") {
      gg <- df.active() %>% ggplot(aes(x=day)) +
        geom_area(aes(y = cases.total,                 fill = "recovered")) +
        geom_area(aes(y = cases.active + deaths.total, fill = "active"))    +
        geom_area(aes(y = deaths.total,                fill = "death"))     +
        theme(
          legend.position = c(0.02, 0.98),
          legend.justification = c("left", "top")
        ) +
        facet_wrap(
          ~country.iso,
          ncol = ncol
        ) +
        scale_fill_manual(name="Cases (total number)",
                          values = c("recovered"="#00ba38",
                                     "active"="#f8766d",
                                     "death"="dark grey"))  # line color
    }
    
    if(input$data.selected == "normalized cases per 100.000 residents") {
      gg <- df.active() %>% ggplot(aes(x=day)) +
        geom_area(aes(y = cases.total/population*100000                  , fill = "recovered")) +
        geom_area(aes(y = (cases.active + deaths.total)/population*100000, fill = "active"))    +
        geom_area(aes(y = deaths.total/population*100000                 , fill = "death"))     +
        theme(
          legend.position = c(0.02, 0.98),
          legend.justification = c("left", "top")
        ) +
        facet_wrap(
          ~country.iso,
          ncol = ncol
        ) +
        # ylim(0, 700) +
        scale_fill_manual(name="cumulated incidences\nper 100.000 residents",
                          values = c("recovered"="#00ba38",
                                     "active"="#f8766d",
                                     "death"="dark grey"))  # line color
      
    }
    return(gg)
    
  }
  
  generateWorldActive <- function() {
    gg <- df.world() %>% ggplot(aes(x=day)) +
      geom_area(aes(y = cases.total,  fill = "recovered")) +
      geom_area(aes(y = cases.active, fill = "active"))    +
      geom_area(aes(y = deaths.total, fill = "death"))     +
      theme(
        legend.position = c(0.02, 0.98),
        legend.justification = c("left", "top")
      ) +
      scale_fill_manual(name="Cases (total number)",
                        values = c("recovered"="#00ba38",
                                   "active"="#f8766d",
                                   "death"="dark grey"))  # line color
  }
  
  generateWorldIncidents <- function() {
    gg <- df.world() %>% 
      mutate(
        incidents.total  = cases.total/population*100000,
        incidents.active = cases.active/population*100000,
        mortality        = deaths.total/population*100000
      ) %>%
      ggplot(aes(x=day)) +
      geom_area(aes(y = incidents.total,  fill = "recovered")) +
      geom_area(aes(y = incidents.active, fill = "active"))    +
      geom_area(aes(y = mortality, fill = "death"))     +
      theme(
        legend.position = c(0.02, 0.98),
        legend.justification = c("left", "top")
      ) +
      scale_fill_manual(name="cumulated incidences\nper 100.000 residents",
                        values = c("recovered"="#00ba38",
                                   "active"="#f8766d",
                                   "death"="dark grey"))  # line color
  }
  
  generateCountriesActive <- function(){
    gg <- df.latest()                                                     %>% 
      # dplyr::filter(charcode %in% coi)                                    %>%
      ungroup(charcode)                                                   %>%
      top_n(20, cases.total)                                                     %>%
      ggplot(aes(reorder(country.iso, cases.total), cases.total)) + 
      geom_bar(stat="identity", fill = "#f8766d")               +
      coord_flip()                            +
      geom_text(aes( y     = cases.total, 
                     label = paste(
                       format(round(cases.total/1000,0), big.mark=".", decimal.mark=","),
                       "k")
      ), 
      hjust = "middle", 
      check_overlap = TRUE,
      color = "black")              +
      labs( x = "Land", 
            y = "Anzahl Erkrankte")
    return(gg)
  } 
  
  generateCountriesIncidents <- function(){
    gg <- df.latest()                                                     %>% 
      # dplyr::filter(charcode %in% coi)                                    %>%
      mutate(Rsum = cases.total/population*100000)               %>% 
      select(c(charcode, country.iso, Rsum, cases.total, population)) %>% 
      ungroup(charcode)                                                   %>%
      top_n(20, Rsum)                                                     %>%
      ggplot(aes(reorder(country.iso, Rsum), Rsum)) + 
      geom_bar(stat="identity")               +
      coord_flip()                            +
      geom_text(aes( y     = Rsum-100, 
                     label = round(Rsum,0)), 
                color = "white")              +
      labs( x = "Land", 
            y = "Anzahl Erkrankte pro 100.000 Einwohner")
    return(gg)
  } 
  
  
  observeEvent(input$load.data, {
    cat("hallo world load.data!\n")
    cat(getwd())
    df <<- loadData()
    save(df, file = "data//cases.rda")
  })
  
  # day <- as.Date("2020-05-28")  
  day <- reactive({return(input$date.snapshot)})
  
#---------------------------------------
# Dashboard
#---------------------------------------
  output$world.total.cases <- renderValueBox(
    valueBox(
      h2("total cases"),
      h3(format(world.cases("cases.total", day())[[2]], big.mark=".", decimal.mark=",")),
      "as.character(input$date.snapshot)",
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "blue"
    )
  )  
  
  output$world.active.cases <- renderValueBox(
    valueBox(
      h2("active cases"),
      h3(format(world.cases("cases.active", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "red"
    )
  )  
  
  output$world.recovered.cases <- renderValueBox(
    valueBox(
      h2("recovered cases"),
      h3(format(world.cases("cases.recovered", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "green"
    )
  )  
  
  output$world.death <- renderValueBox(
    valueBox(
      h2("total deaths"),
      h3(format(world.cases("deaths.total", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "black"
    )
  )  
  
  output$world.active <- renderPlot({
    plot(generateWorldActive())
    cat(str(input))
  })
  
  output$world.incidents <- renderPlot({
    plot(generateWorldIncidents())
  })

  output$countries.active <- renderPlot({
    plot(generateCountriesActive())
  })

  output$countries.incidents <- renderPlot({
    plot(generateCountriesIncidents())
  })
  
#---------------------------------------
# Active Cases
#---------------------------------------
  output$active <- renderPlot({
    #-----------------------------------------------------------
    # Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
    #   - gefiltert nach Tag
    #-----------------------------------------------------------
    plot(generateActive())
    # plot(generateActive(c("Germany", "United States")))
    # plot(generateActive(input$countryId, input$data.attribute))
    
    
  })
  
  # output$click_info <- renderText({
  #   # paste0("x=", input$active_click$x, "\ny=", input$active_click$y)
  #   nearPoints(df.active(), input$active_click, xvar="day", yvar="cases.total")
  # 
  # })
  # 
  # output$hover_info <- renderText({
  #   paste0("x=", input$active_hover$x, "\ny=", input$active_hover$y)
  # })
  

  # output$active2 <- renderPlot({
  #   #-----------------------------------------------------------
  #   # Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
  #   #   - gefiltert nach Tag
  #   #-----------------------------------------------------------
  #   plot(generateActive(input$countryId2))
  #   # plot(generateActive(input$countryId2), input$data.attribute)
  # })
}

