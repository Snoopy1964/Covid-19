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
# library("geojsonio")
library("wppExplorer")
library(shiny)
library(shinydashboard)
library(DT)

#-----------------------------------------------------
# initialization
#-----------------------------------------------------
cat("---------------------> Initialization Server.R\n")
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
# 
# 
#-----------------------------------------------------
# setup global variables
#-----------------------------------------------------
## countries <- loadCountries()

# dataInput <- function() {
#   if( exists("df")) return(df)
#   if(!exists("df")){ 
#     df <- loadData()
#     return(df)
#   }
# }
print(getwd())
load("data//cases.rda", .GlobalEnv)
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
             summarize(cases      = sum(cases),
                       active     = sum(active, na.rm = TRUE),
                       deaths     = sum(deaths, na.rm = TRUE),
                       population = sum(population, na.rm = TRUE))
    )
  })
  df.day <- reactive({
    # return( df %>% group_by(charcode)  %>% dplyr::filter(day == input$date.snapshot))
    return( df %>% dplyr::filter(day == input$date.snapshot))
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
        geom_area(aes(y = cases,           fill = "recovered")) +
        geom_area(aes(y = active + deaths, fill = "active"))    +
        geom_area(aes(y = deaths,          fill = "death"))     +
        theme(
          legend.position = "top",
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
        geom_area(aes(y = cases/population*100000            , fill = "recovered")) +
        geom_area(aes(y = (active + deaths)/population*100000, fill = "active"))    +
        geom_area(aes(y = deaths/population*100000           , fill = "death"))     +
        theme(
          # legend.position = c(0.02, 0.98),
          legend.position = "top",
          legend.justification = c("left", "top")
        ) +
        facet_wrap(
          ~country.iso,
          ncol = ncol
        ) +
        # ylim(0, 700) +
        scale_fill_manual(name="cumulated incidences per 100.000 residents",
                          values = c("recovered"="#00ba38",
                                     "active"="#f8766d",
                                     "death"="dark grey"))  # line color
      
    }
    return(gg)
    
  }
  
  generateWorldActive <- function() {
    print(df.world())
    gg <- df.world() %>% ggplot(aes(x=day)) +
      geom_area(aes(y = cases,  fill = "recovered")) +
      geom_area(aes(y = active, fill = "active"))    +
      geom_area(aes(y = deaths, fill = "death"))     +
      theme(
        legend.position = c(0.02, 0.98),
        legend.justification = c("left", "top")
      ) +
      scale_fill_manual(name="Cumulated Cases (total numbers)",
                        values = c("recovered"="#00ba38",
                                   "active"="#f8766d",
                                   "death"="dark grey"),
                        labels = c("cum. deaths", 
                                   "cum. recovered cases",
                                   "cum. cases"))  # line color
  }
  
  generateWorldIncidents <- function() {
    gg <- df %>% 
      group_by(day)                           %>%
      dplyr::filter(day >= "2020-02-01")      %>%
      summarize(
        cases.day     = sum(cases.day),
        active.day    = sum(active.day),
        recovered.day = sum(recovered.day),
        deaths.day    = sum(deaths.day)
      )                                       %>%
      ggplot(aes(x=day))
    
    return(gg + 
      geom_area(aes(y=cases.day,                fill = "total"),     stat="identity") +
      geom_area(aes(y=deaths.day+recovered.day, fill = "recovered"), stat="identity") +
      geom_area(aes(y=deaths.day,               fill = "death"),     stat="identity") +
      theme(
        legend.position = c(0.02, 0.98),
        legend.justification = c("left", "top")
      ) +
      scale_fill_manual(  name="Cumulated Incidences per day",
                          # breaks = c(1,2,3),
                          values = c("total"="#f8766d",
                                     "recovered"="#00ba38",
                                     "death"="dark grey"),
                          labels = c("new deaths per day", 
                                     "new recovered per day",
                                     "new cases per day"))  # line color
    )
    
    # gg <- df.world() %>% 
    #   mutate(
    #     incidents.total  = cases/population*100000,
    #     incidents.active = active/population*100000,
    #     mortality        = deaths/population*100000
    #   ) %>%
    #   ggplot(aes(x=day)) +
    #   geom_area(aes(y = incidents.total,  fill = "recovered")) +
    #   geom_area(aes(y = incidents.active, fill = "active"))    +
    #   geom_area(aes(y = mortality, fill = "death"))     +
    #   theme(
    #     legend.position = c(0.02, 0.98),
    #     legend.justification = c("left", "top")
    #   ) +
    #   scale_fill_manual(name="cumulated incidences\nper 100.000 residents",
    #                     values = c("recovered"="#00ba38",
    #                                "active"="#f8766d",
    #                                "death"="dark grey"))  # line color
  }
  
  generateCountriesActive <- function(){
    gg <- df.day()                                                     %>% 
      # dplyr::filter(charcode %in% coi)                                    %>%
      # ungroup(charcode)                                                   %>%
      top_n(20, cases)                                                     %>%
      ggplot(aes(reorder(country.iso, cases), cases)) + 
      geom_bar(stat="identity", fill = "#f8766d")               +
      coord_flip()                            +
      geom_text(aes( y     = cases, 
                     label = paste(
                       format(round(cases/1000,0), big.mark=".", decimal.mark=","),
                       "k")
      ), 
      hjust = "top", 
      check_overlap = TRUE,
      color = "black")              +
      labs( x = "Land", 
            y = "Anzahl Erkrankte")
    return(gg)
  } 
  
  generateCountriesIncidents <- function(){
    gg <- df.day()                                                     %>% 
      # dplyr::filter(charcode %in% coi)                                    %>%
      mutate(Rsum = cases/population*100000)               %>% 
      select(c(charcode, country.iso, Rsum, cases, population)) %>% 
      # ungroup(charcode)                                                   %>%
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
    # load countries
    countries <<- loadCountries()                 # <<- : load in .GlobalEnv
    save(countries, file = "data//countries.rda")
    
    #load data for list of countries
    df <<- loadData()                             # <<- : load in .GlobalEnv
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
      h3(format(world.cases("cases", day())[[2]], big.mark=".", decimal.mark=",")),
      "as.character(input$date.snapshot)",
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "blue"
    )
  )  
  
  output$world.active.cases <- renderValueBox(
    valueBox(
      h2("active cases"),
      h3(format(world.cases("active", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "red"
    )
  )  
  
  output$world.recovered.cases <- renderValueBox(
    valueBox(
      h2("recovered cases"),
      h3(format(world.cases("recovered", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "green"
    )
  )  
  
  output$world.death <- renderValueBox(
    valueBox(
      h2("total deaths"),
      h3(format(world.cases("deaths", day())[[2]], big.mark=".", decimal.mark=",")),
      icon  = icon('export', lib = 'glyphicon'),#icon("sign-in"),
      color = "black"
    )
  )  
  
  output$world.active <- renderPlot({
    gg <- generateWorldActive()
    gg
    # cat(str(input))
  })
  
  output$world.incidents <- renderPlot({
    print(df)
    gg <- generateWorldIncidents()
    gg
  })

  output$countries.active <- renderPlot({
    gg <- generateCountriesActive()
    gg
  })

  output$countries.incidents <- renderPlot({
    gg <- generateCountriesIncidents()
    gg
  })

#---------------------------------------
# Statistics
#---------------------------------------
  output$cases.countries <- DT::renderDataTable({
    df.tmp <- df.day() %>% select(country.iso, cases, active, recovered, deaths, population)
    print(df.tmp)
    DT::datatable(
      df.tmp, 
      options = list(
        lengthMenu = c(5, 10, 20, 50),
        pageLength = 10,
        # language   = list(thousands = "."),
        order      = list(list(2,'desc'))
      )) %>% 
      formatRound(names(df.tmp)[-1], 0)
  })
    
#---------------------------------------
# Active Cases
#---------------------------------------
  output$active <- renderPlot({
    #-----------------------------------------------------------
    # Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
    #   - gefiltert nach Tag
    #-----------------------------------------------------------
    gg <- generateActive()
    gg
  })
  
  # output$click_info <- renderText({
  #   paste("x=", input$active_click$x, "\ny=", input$active_click$y)
  #   # nearPoints(df.active(), input$active_click, xvar="day", yvar="cases.total")
  #   
  # })
  # 
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
  
  # output$hover_info <- renderPrint({
  #   day.hover <- as.character(as.Date(as.numeric(input$active_hover$x), origin="1970-01-01"))
  #   iso.hover <- as.character(input$active_hover$panelvar1)
  #   df.tmp <- df.active() %>% dplyr::filter(day == day.hover & country.iso == iso.hover)
  #   print(paste("Datum: ",day.hover, "\n"))
  #   print(df.tmp)
  #   cat("df.tmp: ", df.tmp$day, df.tmp$cases.total)
  #   cat("input$active_hover:\n")
  #   str(input$active_hover)
  # })
  
  
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
                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    
    
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

