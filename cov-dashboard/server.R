#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#------------------------------------------------------
# ToDo:
# - https://github.com/Snoopy1964/Covid-19/issues/3
#
#------------------------------------------------------
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
library(shiny)
library(shinydashboard)
library(DT)

#-----------------------------------------------------
# initialization
#-----------------------------------------------------
cat("---> Initialization Server.R\n")
# print(environment())
# print(getwd())


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
# 
# 
#-----------------------------------------------------
# setup global variables
#-----------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # df <- load("data//cases.rda")
  debug.on   <<- options("Debug.Dashboard")[[1]]      # <<- loads in .GlobalEnv
  cat("------> initialize server(): Debugging Mode:", debug.on, "\n")
  force.load <<- options("Force.Load")[[1]]           # <<- loads in .GlobalEnv
  cat("------> initialize server(): force data load:", force.load, "\n")
  
  
  #--------------------------------------------------------------
  # definition of data functions to be used inside server only!
  #--------------------------------------------------------------

  df.input <- reactive({
    dummy <- input$load.data
    if(debug.on) cat("------------> df.input() load data from file\n")
    load("data/cases.rda")
    last.day <- as.Date((df %>% summarize(day = max(day)))[[1]])
    if (last.day < today() | force.load) {
      df <- loadData()
      save(df, file = "data/cases.rda")
      if(debug.on) cat("------------> data saved to file\n")
    }
    if(debug.on) cat("----end-----> df.input() load data from file\n")
    return(df)
  })
  
  df.active <- reactive({
    return(df.input() %>% 
             dplyr::filter(charcode %in% 
            # dplyr::filter(country.iso %in% 
                             input$countryId)
    )
  })

  df.region <- reactive({
    if(debug.on) cat("---------> df.region()\n")
    region <- getGrouping(input$region.select)
    ds.tmp <- df.input()                                            %>%
      dplyr::filter(charcode != "AA")                               %>%  # remove entry for World (charcode AA)
      group_by(day, .data[[region]])                                %>%
      summarize(cases         = sum(cases),
                cases.day     = sum(cases.day),
                active        = sum(active, na.rm = TRUE),
                active.day    = sum(active.day, na.rm = TRUE),
                recovered     = sum(recovered, na.rm = TRUE),
                recovered.day = sum(recovered.day, na.rm = TRUE),
                deaths        = sum(deaths, na.rm = TRUE),
                deaths.day    = sum(deaths.day, na.rm = TRUE),
                population    = sum(population, na.rm = TRUE)
      )                                                             %>%
      # South Sudan and Western Sahara have no entry in GEO3 -> github issue #3
      dplyr::filter(!is.na(.data[[region]]))                        %>%
      # rename grouping/aggregating variable to Region
      rename(Region = .data[[region]])
    return(ds.tmp)
  })
  
  df.day <- reactive({
    # dummy <- input$load.data
    return( df.input() %>% dplyr::filter(day == input$date.snapshot))
  })
  
  df.country <- reactive({
    selected.country <- input$selectCountry
    df.tmp  <- df.input()                      %>% 
      dplyr::filter(charcode != "AA")          %>% # remove entry for World (charcode AA)
      group_by(day)                            %>% 
      dplyr::filter(charcode == selected.country)
  })
  
  country.cases <- function(sum.attribute) {
    if(debug.on) cat("------------> country.cases()", input$date.snapshot, input$selectCountry,sum.attribute, "\n")
    df.tmp  <- df.input()                     %>% 
      dplyr::filter(day == input$date.snapshot, charcode == input$selectCountry) %>%
      rename(
        total.number = .data[[sum.attribute]],
        day.number   = .data[[paste(sum.attribute,"day",sep=".")]])                    
    if(debug.on) cat("------end---> country.cases()\n")
    return(df.tmp)
  }
  
  # ----------------------------------------------------------
  # manipulate UI elements
  # ----------------------------------------------------------
  observe({
    if(debug.on) cat("------> observe()\n")
    updateSelectInput(
      session,
      "selectCountry",
      choices = country.selector(df.day()),
      selected = "AA"
    )
    
    updateSelectInput(
      session,
      "countryId",
      choices = country.selector(df.day(), addNr = FALSE),
      selected = c("AA","DE", "US", "IN")
      # selected = c("World",
      #              "Germany", 
      #              "United States",
      #              "United Kingdom")
    )
    
  })
  #----------------------------------------------------------
  # generate plots to be used inside server output
  #----------------------------------------------------------
  generateChartsOverview <- function(selected.country = "AA", ma = -0.02){
    # default selection is World (charcode = AA)
    if(debug.on) {
      paste("------------> selected.country:", selected.country, "\n")
    }
    start.date <- as.Date("2020-02-01")
    ds.tmp <- df.input()               %>%
      dplyr::filter(day >= start.date, charcode == selected.country)  
    
    norm.factor <- 100000/as.numeric(ds.tmp %>% summarize(population = max(population)))
    # Selected Country (oder World) summiert
    gg1 <- ds.tmp                                           %>% 
      ggplot(aes(x=day))                                     +
      geom_area(aes(y = cases,          fill = "active"))    +
      geom_area(aes(y = cases - active, fill = "recovered")) +
      geom_area(aes(y = deaths,         fill = "death"))     +
      geom_line(aes(y=cases), color = "blue", size = 1)      +
      annotate(
        geom  = "text",
        x     = mean(range(ds.tmp$day)),
        y     = range(ds.tmp$cases)[2],
        label = "Cases (total number)",
        hjust = 0.5,
        vjust = 1,
        size  = 5
      )                                                      +
      theme(
        legend.position = c(0.01, 0.96),
        legend.justification = c("left", "top"),
        plot.margin          = unit(c(0,ma,ma,0), "cm"),
        axis.text.x          = element_blank(),
        axis.ticks.x         = element_blank()
      ) +
      labs( x     = NULL,
            y     = NULL,
            title = NULL)                   +
      scale_y_continuous(
        labels = label_number_si(),
        sec.axis = sec_axis(~ . * norm.factor, name = "per 100k", labels = label_number_si())
      )                                                      +
      scale_fill_manual(
        name=NULL,
        values = c("recovered"="#00ba38",
                   "active"="#f8766d",
                   "death"="grey30")
      )  
    
    gg2 <- ds.tmp                                  %>% 
      dplyr::filter(day >= start.date)             %>%
      mutate(mortality = deaths/cases)             %>% 
      ggplot(aes(x = day))                          + 
      geom_area(aes(y=mortality, fill = "death"))   + 
      geom_smooth(aes(y=mortality), span = 0.1)     + 
      annotate(
        geom  = "text",
        x     = mean(range(ds.tmp$day)),
        y     = 0.16,
        label = "Mortality (Deaths/Cases) in %",
        hjust = 0.5,
        vjust = 1,
        size  = 5
      )                                                      +
      theme(
        legend.position = "none",
        plot.margin     = unit(c(ma,ma,ma,0), "cm") ,
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank()
      )                                             +
      labs( x     = NULL,
            y     = NULL,
            title = NULL
      )                                             +
      scale_y_continuous(
        labels = label_percent(),
        limits = c(0, 0.16),
        sec.axis = sec_axis(
          ~ . , 
          name   = "Percent", 
          labels = label_percent())
      )                           +
      scale_fill_manual(  name=NULL,
                          values = c("death"="grey50"),
                          labels = c("mortatlity")
      )                                
    
    
    # Selected Country (oder World) summiert
    gg.data <- ds.tmp %>% 
      ggplot(aes(x=day))                       +                 
      theme(
        legend.position = "none",
        plot.margin     = unit(c(ma,ma,0,0), "cm"),
      )                                        +
      labs( x     = NULL,
            y     = NULL,
            title = NULL
      )                                        +
      scale_y_continuous(
        labels  = label_number_si(),
        sec.axis = sec_axis(
          ~ . * norm.factor * 10, 
          name   = "per 1 Mio.", 
          labels = label_number_si())
      )                           +
      scale_fill_manual(  name=NULL,
                          values = c("total"="#f8766d"),
                          labels = c("new cases per day")
      )
    
    
    gg3 <- gg.data     +
      geom_area(aes(y=cases.day,             fill = "total"),     stat="identity") +
      geom_smooth(aes(y=cases.day), span = 0.1) +
      annotate(
        geom  = "text",
        x     = mean(range(ds.tmp$day)),
        y     = range(ds.tmp$cases.day)[2],
        label = "New Cases per day",
        hjust = 0.5,
        vjust = 1,
        size  = 5
      )                                                                       
    
    gg4 <- gg.data +
      geom_smooth(aes(y=cases.day))
    
    gg <- gg1 + gg2 + gg3 + plot_layout(nrow=3)
    return(gg)
  }

  generateChartsRegions <- function(ma = -0.02){
    # start.date <- as.Date("2020-02-01")
    # ds.tmp <- df.world()               %>%
    #   dplyr::filter(day >= start.date)  
    
    if(debug.on) cat("---------> generateChartsRegion(): region selected", input$region.select, "\n")
    gg <- ggarrange(
      # generateRegionsCases(input$region.select),
      # generateRegionsIncidents(input$region.select),
      # generateRegionsDeaths(input$region.select),
      # generateRegionsMortality(input$region.select),
      generateRegionsCases(),
      generateRegionsIncidents(),
      generateRegionsDeaths(),
      generateRegionsMortality(),
      ncol = 2,
      nrow = 2,
      widths = c(1,1)
    )
    return(gg)
  }
  

  
  #-------------------------------------------------------
  # generate plots for Tab summary.charts.regions
  #-------------------------------------------------------
  getGrouping <- function(i) {
    if(debug.on) cat("selected region:", i, str(i), "\n")
    if(i==0) {
      return("country.iso")
    } else if(i ==1) {
      return("REGION")
    } else if(i ==2) {
      return("continent")
    } else if(i ==3) {
      return("GEO3major")
    } else if(i == 4) {
      return("GEO3")
    }
  }
  
  generateRegionsCases <- function(){
    title.text <- paste("Anzahl Erkrankte pro Land am", input$date.snapshot, "Top 20)")
    gg <- df.region()                                           %>% 
      dplyr::filter(day == input$date.snapshot)                 %>%
      # Definition of plot
      top_n(20, cases)                                          %>%
      ggplot(aes(reorder(Region, cases), cases)) + 
      geom_bar(stat="identity", fill = "#0073b7", alpha = 0.6)  +
      coord_flip()                                              +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(Region, 20), 
                  cases), 
          cases)                                                +
      # add numbers to the bars
      geom_text_repel(aes( y     = cases, 
                           label = format(cases, big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                          +
      labs( x = NULL,
            y = NULL,
            # y = "Anzahl Erkrankte",
            title = title.text)                                 +
      theme(
        plot.title   = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 

  generateRegionsIncidents <- function(region.selected){
    title.text <- paste("Anzahl Erkrankte pro 100.000 Einwohner pro Land am", input$date.snapshot, "Top 20)")
    gg <- df.region()                                           %>% 
      dplyr::filter(day == input$date.snapshot)                 %>%
      mutate(Rsum = cases/population*100000)                    %>% 
      select(c(Region, Rsum, cases, population)) %>% 
      top_n(20, Rsum)                                           %>%
      # Definition of plot
      ggplot(aes(reorder(Region, Rsum), Rsum))        + 
      geom_col(fill = "#0073b7", alpha = 0.6)         +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(Region, 20), 
                  Rsum), 
          Rsum)                                       +
      # add numbers to the bars
      geom_text_repel(aes( y     = Rsum, 
                           label = format(round(Rsum,0), big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( title = title.text,
            x = NULL, 
            y = NULL
            # y = "Anzahl Erkrankte pro 100.000 Einwohner"
      )                                               +
      theme(
        plot.title   = element_text(hjust = 0.0, size = 12, face = "bold",),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  generateRegionsDeaths <- function(region.selected){
    title.text <- paste("Cumulated Deaths on ", today()-1)
    gg <- df.region()                                             %>% 
      dplyr::filter(day == input$date.snapshot)                   %>%
      select(c(Region, deaths))                                   %>% 
      top_n(20, deaths)                                           %>%
      # Definition of plot
      ggplot(aes(reorder(Region, deaths), deaths))   + 
      geom_col(fill = "grey60")     +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(Region, 20), deaths), deaths)              +
      # add numbers to the bars
      geom_text_repel(aes( y     = deaths, 
                           label = format(deaths, big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( title = title.text,
            x = NULL, 
            y = NULL
      )                                               +
      theme(
        plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  generateRegionsMortality <- function(region.selected){
    title.text <- paste("Mortality Rate on ", today()-1)
    gg <- df.region()                                           %>% 
      dplyr::filter(day == input$date.snapshot)                 %>%
      select(c(Region, deaths,cases))                           %>% 
      mutate(mortality = deaths/cases)                          %>%
      # Definition of plot
      top_n(20, mortality)                                      %>%
      ggplot(aes(reorder(Region, mortality), mortality))        + 
      geom_col(fill = "grey60")                                 +
      coord_flip()                                              +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(Region, 20), mortality), mortality)              +
      # add numbers to the bars
      geom_text_repel(aes( y     = mortality, 
                           label = paste(format(mortality*100, digits = 3, big.mark = ".", decimal.mark = ","),"%")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                          +
      labs( title = title.text,
            x = NULL, 
            y = NULL
      )                                                         +
      theme(
        plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  
  #-------------------------------------------------------
  # generate plots for Tab summary.charts.countries.top20
  #-------------------------------------------------------
  generateCountriesCases <- function(){
    title.text <- paste("Anzahl Erkrankte pro Land am", input$date.snapshot, "Top 20)")
    gg <- df.day()                                              %>% 
      top_n(20, cases)                                          %>%
      # Definition of plot
      ggplot(aes(reorder(country.iso, cases), cases)) + 
      geom_bar(stat="identity", fill = "#0073b7", alpha = 0.6)     +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(country.iso, 20), 
                  cases), 
          cases)                                       +
      # add numbers to the bars
      geom_text_repel(aes( y     = cases, 
                           label = format(cases, big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( x = NULL,
            y = NULL,
            # y = "Anzahl Erkrankte",
            title = title.text)                       +
      theme(
        plot.title   = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  generateCountriesIncidents <- function(){
    title.text <- paste("Anzahl Erkrankte pro 100.000 Einwohner pro Land am", input$date.snapshot, "Top 20)")
    gg <- df.day()                                              %>% 
      mutate(Rsum = cases/population*100000)                    %>% 
      select(c(charcode, country.iso, Rsum, cases, population)) %>% 
      top_n(20, Rsum)                                           %>%
      # Definition of plot
      ggplot(aes(reorder(country.iso, Rsum), Rsum))   + 
      geom_col(fill = "#0073b7", alpha = 0.6)         +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(country.iso, 20), 
                  Rsum), 
          Rsum)                                       +
      # add numbers to the bars
      geom_text_repel(aes( y     = Rsum, 
                           label = format(round(Rsum,0), big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( title = title.text,
            x = NULL, 
            y = NULL
            # y = "Anzahl Erkrankte pro 100.000 Einwohner"
      )                                               +
      theme(
        plot.title   = element_text(hjust = 0.0, size = 12, face = "bold",),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  generateCountriesDeaths <- function(){
    title.text <- paste("Cumulated Deaths on ", today()-1)
    gg <- df.day()                                              %>% 
      select(c(charcode, country.iso, deaths)) %>% 
      # Definition of plot
      top_n(20, deaths)                                           %>%
      ggplot(aes(reorder(country.iso, deaths), deaths))   + 
      geom_col(fill = "grey60")     +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(country.iso, 20), deaths), deaths)              +
      # add numbers to the bars
      geom_text_repel(aes( y     = deaths, 
                           label = format(deaths, big.mark = ".", decimal.mark = ",")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( title = title.text,
            x = NULL, 
            y = NULL
      )                                               +
      theme(
        plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 
  
  generateCountriesMortality <- function(){
    title.text <- paste("Mortality Rate on ", today()-1)
    gg <- df.day()                                              %>% 
      select(c(charcode, country.iso, deaths,cases))            %>% 
      mutate(mortality = deaths/cases)                          %>%
      # Definition of plot
      top_n(20, mortality)                                         %>%
      ggplot(aes(reorder(country.iso, mortality), mortality))   + 
      geom_col(fill = "grey60")     +
      coord_flip()                                    +
      # wrap axis.text for long country names like "Holy See (Vatican City State)"
      aes(reorder(stringr::str_wrap(country.iso, 20), mortality), mortality)              +
      # add numbers to the bars
      geom_text_repel(aes( y     = mortality, 
                           label = paste(format(mortality*100, digits = 3, big.mark = ".", decimal.mark = ","),"%")), 
                      hjust = "top",
                      direction = "x",
                      color = "black")                +
      labs( title = title.text,
            x = NULL, 
            y = NULL
      )                                               +
      theme(
        plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    return(gg)
  } 

  day <- reactive({return(input$date.snapshot)})
  
#-----------------------------------------------------
# Generation of plots for "Cases, Incidences & Deaths
#-----------------------------------------------------
  generateCompare <- function() {
    # temporary!!!!!!
    ncol <- 2
    
    if(input$data.selected == "absolute cases") {
      gg <- df.active() %>% ggplot(aes(x=day)) +
        geom_area(aes(y = cases,           fill = "recovered")) +
        geom_area(aes(y = active + deaths, fill = "active"))    +
        geom_area(aes(y = deaths,          fill = "death"))     +
        theme(
          # legend.position = c(0.02, 0.98),
          legend.position = "left",
          legend.justification = c("left", "top")
        ) +
        facet_wrap(
          ~country.iso,
          ncol = ncol
        ) +
        xlim(input$date.range, today()) +
        scale_fill_manual(name="Cases (total number)",
                          values = c("recovered"="#00ba38",
                                     "active"="#f8766d",
                                     "death"="dark grey"))  # line color
    }
    
    if(input$data.selected == "normalized cases per 100.000 residents") {
      gg <- df.active() %>% ggplot(aes(x=day)) +
        geom_area(aes(y = cases/population*100000            , fill = "recovered")) +
        geom_area(aes(y = (active + deaths)/population*100000, fill = "active"))    +
        geom_area(aes(y = deaths/population*100000           , fill = "death"))     +
        theme(
          legend.position = c(0.02, 0.98),
          # legend.position = "top",
          legend.justification = c("left", "top")
        ) +
        facet_wrap(
          ~country.iso,
          ncol = ncol
        ) +
        xlim(input$date.range, today()) +
        scale_fill_manual(name="cumulated incidences per 100.000 residents",
                          values = c("recovered"="#00ba38",
                                     "active"="#f8766d",
                                     "death"="dark grey"))  # line color
      
    }
    return(gg)
    
  }
  
  
  
#---------------------------------------
# Dashboard
#---------------------------------------
  
  output$region <- renderText(country.iso.name.get_by_charcode(input$selectCountry))

  
  output$total.cases <- renderValueBox({
    if(debug.on) cat("---------> output$total.cases\n" )
    cases.tmp    <- country.cases("cases")
    total.number <- cases.tmp$total.number[1]
    day.number   <- cases.tmp$day.number[1]
    valueBox(
      h4("cases"),
      tagList(
        h4(format.number(total.number)),
        h6(paste("(", format.number(day.number),")"))
      ),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "blue"
    )
  })  
  
  output$active.cases <- renderValueBox({
    if(debug.on) cat("---------> output$active.cases\n" )
    cases.tmp    <- country.cases("active")
    total.number <- cases.tmp$total.number[1]
    day.number   <- cases.tmp$day.number[1]
    valueBox(
      h4("active"),
      tagList(
        h4(format.number(total.number)),
        h6(paste("(", format.number(day.number),")"))
      ),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "red"
    )
  })  
  
  output$recovered.cases <- renderValueBox({
    if(debug.on) cat("---------> output$recovered.cases\n" )
    cases.tmp    <- country.cases("recovered")
    total.number <- cases.tmp$total.number[1]
    day.number   <- cases.tmp$day.number[1]
    valueBox(
      h4("recovered"),
      tagList(
        h4(format.number(total.number)),
        h6(paste("(", format.number(day.number),")"))
      ),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "green"
    )
  })  
  
  output$deaths <- renderValueBox({
    if(debug.on) cat("---------> output$deaths\n" )
    cases.tmp    <- country.cases("deaths")
    total.number <- cases.tmp$total.number[1]
    day.number   <- cases.tmp$day.number[1]
    valueBox(
      h4("deaths"),
      tagList(
        h4(format.number(total.number)),
        h6(paste("(", format.number(day.number),")"))
      ),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "black"
    )
  })  

  output$summary.charts <- renderPlot({
    if(debug.on) cat("---------> generateChartsOverview() for", input$selectCountry,"\n" )
    generateChartsOverview(input$selectCountry)
  })
  
  output$summary.map <- renderText({
    "Coming soon...."
  })
  #---------------------------------------
  # Top Regions
  #---------------------------------------
  
  output$summary.charts.regions <- renderPlot({
    generateChartsRegions()  
  })
  
  output$summary.charts.countries.top20 <- renderPlot({
    ggarrange(
      generateCountriesCases(),
      generateCountriesIncidents(),
      generateCountriesDeaths(),
      generateCountriesMortality(),
      ncol = 2,
      nrow = 2,
      widths = c(1,1)
    )
  })
  


  #---------------------------------------
  # Statistics (day)
  #---------------------------------------
  output$cases.countries <- DT::renderDataTable({
    df.tmp <- df.day() %>% select(country.iso, cases, cases.day, active, recovered, deaths, population)
    cat("----------------------> DT::renderDataTable()")
    print(df.tmp)
    DT::datatable(
      df.tmp,
      rownames = FALSE,
      options = list(
        lengthMenu = c(5, 10, 20, 50),
        pageLength = 20,
        # language   = list(thousands = "."),
        order      = list(list(1,'desc')),
        columnDefs = list(list(width = '100px', targets = c(1)))
      )) %>% 
      formatRound(names(df.tmp)[-1], 0)
  })
  

#---------------------------------------
# Active Cases
#---------------------------------------
  output$compare.countries.active <- renderPlot({
    #-----------------------------------------------------------
    # Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
    #   - gefiltert nach Tag
    #-----------------------------------------------------------
    gg <- generateCompare()
    gg
  })
  
  output$click_info <- renderPrint({
    if(is.null(input$active_click)) return(NULL)
    
    day.click <- as.character(as.Date(as.numeric(input$active_click$x), origin="1970-01-01"))
    iso.click <- as.character(input$active_click$panelvar1)
    df.tmp <- df.active() %>% dplyr::filter(day == day.click & country.iso == iso.click)
    print(paste("Datum: ",day.click, "\n"))
    print(df.tmp)
    cat("df.tmp: ", df.tmp$day, df.tmp$cases.total)
    cat("input$active_click:\n")
    str(input$active_click)
  })
  
  output$hover_info <- renderUI({
    hover <- input$active_hover
    if(is.null(hover)) return(NULL)
    day.hover <- as.character(as.Date(as.numeric(input$active_hover$x), origin="1970-01-01"))
    iso.hover <- as.character(input$active_hover$panelvar1)

    df.tmp <- df.active() %>% dplyr::filter(day == day.hover & country.iso == iso.hover)
    
    if(nrow(df.tmp) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    # left_px <- hover$range$right #+ left_pct * (hover$range$right - hover$range$left)
    # top_px <- hover$range$top #+ top_pct * (hover$range$bottom - hover$range$top)
    left_px <- hover$range$left 
    top_px  <- hover$range$top 
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", hover$coords_img$x-10, "px; top:", hover$range$top, "px;")
    # "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  
    
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Cases in </b>"  , df.tmp$country.iso, "<br/>",
                    "<b> date: </b>"     , df.tmp$day, "<br/>",
                    "<b> total: </b>"    , format(df.tmp$cases, big.mark = ".", decimal.mark = ","), "<br/>",
                    "<b> recovered: </b>", format(df.tmp$recovered, big.mark = ".", decimal.mark = ","), "<br/>",
                    "<b> deaths: </b>"   , format(df.tmp$deaths, big.mark = ".", decimal.mark = ","), "<br/>",
                    "<b> active: </b>   ", format(df.tmp$active, big.mark = ".", decimal.mark = ","), "<br/>"
      )))
    )
  })
  
  

}

