###################################################
#
# Plots:
#
#
#
#
#
###################################################

# Zur Zeit nicht ausführen!!!!!
if( 1==2 ) { 
  
  #-----------------------------------------
  # Anzahl Erkrankte pro 100.000 Einwohner
  #-----------------------------------------
  coi <- c("BE", "BW", "CA", "DK", "FI", "FR","DE", "GL", "IS", "IN", "IL", "IT", "JP", "LU", "MX", "NL", "NO", "PL", "PT", "RU", "ES", "SE", "CH", "TR", "GB", "US")
  coi <- c("FR","DE", "RU", "US")
  
  # df.latest <- df %>% group_by(charcode)  %>% dplyr::filter(day == today())
  df.latest <- df %>% dplyr::filter(day == today()-1)
  # df.latest <- df %>% dplyr::filter(day == as.Date("2020-06-08"))
  
  gg.incidence.latest <- df.latest                                      %>% 
    mutate(Rsum = cases/population*100000)                        %>% 
    select(c(charcode, country.iso, Rsum, cases, population))     %>% 
    # ungroup(charcode)                                                   %>%
    top_n(20, Rsum)                                                     %>%
    ggplot(aes(reorder(country.iso, Rsum), Rsum))
  
  gg.incidence.latest                       +
    geom_bar(stat="identity", fill = "#f8766d")               +
    coord_flip()                            +
    geom_text_repel(aes( y     = Rsum, 
                         label = round(Rsum,0)), 
                    hjust = "bottom",
                    direction = "x",
                    color = "black")                +
    labs( x = "Land", 
          y = "Anzahl Erkrankte pro 100.000 Einwohner")
  
  
  gg.cases.latest <- df.latest                                      %>% 
    select(c(charcode, country.iso, cases, population))     %>% 
    top_n(20, cases)                                                     %>%
    ggplot(aes(reorder(country.iso, cases), cases))
  
  gg.cases.latest                       +
    geom_bar(stat="identity", fill = "#f8766d")               +
    coord_flip()                            +
    geom_text_repel(aes( y     = cases, 
                         label = format(cases, big.mark = ".", decimal.mark = ",")), 
                    hjust = "top",
                    direction = "x",
                    color = "black")                +
    labs( x = "Land", 
          y = "Anzahl Erkrankte pro 100.000 Einwohner")
  
  #-----------------------------------------------------------
  # Anzahl Aktiver Erkrankungen pro 100.000 Einwohner pro Tag
  #   - gefiltert nach Tag
  #-----------------------------------------------------------
  
  coi <- c("DE")
  
  df.active <- df %>% dplyr::filter(charcode %in% coi) 
  
  gg <- df.active %>% ggplot(aes(x=day))
  gg + 
    geom_area(aes(y = cases,           fill = "recovered")) + 
    geom_area(aes(y = active + deaths, fill = "active"))    +
    geom_area(aes(y = deaths,          fill = "death"))     +
    scale_fill_manual(name="", 
                      values = c("recovered"="#00ba38", "active"="#f8766d", "death"="dark grey"))  # line color
  
  
  #-----------------------------------------------------------
  # CI pro Tag pro 100.000 Einwohner
  #-----------------------------------------------------------
  gg <- df %>% 
    dplyr::filter(charcode %in% c("DE", "US", "BR") & day >= "2020-03-01") %>% 
    group_by(charcode)                                                     %>%
    arrange(charcode,day)                                                  %>% 
    mutate(
      CI   = cases.day/population*100000,
      CI.7 = rollmean(CI,7, align = "right", fill = NA)
    )                                                                      %>%
    select(c(1:4, "CI", "CI.7"))                                           %>%
    ggplot(aes(x=day))
  
  gg + 
    geom_smooth(aes(y=CI, color = charcode)) + 
    geom_point(aes(y=CI.7, color = charcode))
}
##########################################################################
#
#    Dashboard (Overview)
#    - Erkrankungen an Stichtag (selektierbar, default = last_available)
#    - Charts Row 1
#       - total, deaths und active Cases (Anzahl)
#       - Neuerkrankungen pro Tag (smoothed?)
#    - Charts Row 2
#       - selben Plots normiert auf 100.000 Einwohner (für Vergleich)
#
##########################################################################

# (1) load helper functions
source("cov-dashboard//01_helper_func_data.R")
# load countries
if(!exists("countries")) countries <- loadCountries("jhu")


#-----------------------------------------------------------
# define data access function
#-----------------------------------------------------------
debug.on   <- TRUE
force.load <- FALSE
ma         <- 0.2 # Margin von plots in cm

df.input <- function() {
  if(debug.on) cat("------------> df.input()\n")
  if(!exists("ds") | !exists("last.day") | !exists("last.day.load")) {
    if(debug.on) cat("------------> load data from file\n")
    load("data/cases.rda")
    last.day      <- (ds %>% summarise(max.day = max(day)))[[1]]
    
    if(debug.on) print(paste("------------> last.day:",last.day, "\n"))
  }
  if (!exists("last.day"))      last.day <- as.Date("2020-01-01")
  if (!exists("last.day.load")) last.day.load <- last.day
  if ((last.day < today() & last.day.load < today()) | force.load == TRUE) {
    if(debug.on) cat("------------> load data from jhu\n")
    ds <<- loadData()
    last.day      <- (ds %>% summarise(max.day = max(day)))[[1]]
    last.day.load <- today()
    save(ds, last.day, last.day.load, file = "data/cases.rda")
    if(debug.on) cat("------------> data saved to file\n")
  }
  if(debug.on) cat("----end-----> df.input()\n")
  return(ds)
}

df.world <- function() {
  if(debug.on) {
    cat("--------------> df.world()\n")
    print(paste("max date: ",(df.input() %>% summarize(max.day = max(day)))[[1]]))
  }
  ds.tmp <- df.input()      %>% 
    group_by(day) %>% 
    summarize(cases         = sum(cases),
              cases.day     = sum(cases.day),
              active        = sum(active, na.rm = TRUE),
              active.day    = sum(active.day, na.rm = TRUE),
              recovered     = sum(recovered, na.rm = TRUE),
              recovered.day = sum(recovered.day, na.rm = TRUE),
              deaths        = sum(deaths, na.rm = TRUE),
              deaths.day    = sum(deaths.day, na.rm = TRUE),
              population    = sum(population, na.rm = TRUE)
    )
  if(debug.on) {
    print(ds.tmp %>% dplyr::filter(day == today()-1))
    cat("--------------> end of df.world()\n")
  }
  return(ds.tmp)
}

df.regions <- function(region = "REGION") {
  ds.tmp <- ds                       %>%
    group_by(day, .data[[region]])                      %>%
    summarize(cases         = sum(cases),
              cases.day     = sum(cases.day),
              active        = sum(active, na.rm = TRUE),
              active.day    = sum(active.day, na.rm = TRUE),
              recovered     = sum(recovered, na.rm = TRUE),
              recovered.day = sum(recovered.day, na.rm = TRUE),
              deaths        = sum(deaths, na.rm = TRUE),
              deaths.day    = sum(deaths.day, na.rm = TRUE),
              population    = sum(population, na.rm = TRUE)
    )   %>%
    # South Sudan and Western Sahara have no entry in GEO3 -> github issue #3
    dplyr::filter(!is.na(.data[[region]])) %>%
    # rename grouping/aggregating variable to Region
    rename(Region = .data[[region]])
  return(ds.tmp)
}

df.day <- function(){
  return( df.input() %>% dplyr::filter(day == today()-1))
  
}

df.country <- function() {
  # country <- input$selectCountry
  selected.country <- "DE"
  df.tmp  <- df.input()                      %>% 
    group_by(day)                            %>% 
    dplyr::filter(charcode == selected.country)
  return(df.tmp)
}

#-----------------------------------------------------------
# CI pro Tag pro 100.000 Einwohner (World)
#-----------------------------------------------------------

#-------------------------------------------------------
# generate plots for Tab summary.charts.regions
#-------------------------------------------------------
getGrouping <- function(i) {
  if(debug.on) cat("selected region:", i, str(i), "\n")
  if(i==1) {
    return("REGION")
  } else if(i ==2) {
    return("continent")
  } else if(i ==3) {
    return("GEO3major")
  } else if(i == 4) {
    return("GEO3")
  }
}

generateRegionsCases <- function(region.selected = "REGION"){
  day.replace = today()-3
  title.text <- paste("Anzahl Erkrankte pro Land am",  day.replace, "Top 20)")
  gg <- df.regions( region.selected)            %>% 
    dplyr::filter(day == day.replace)                 %>%
    # Definition of plot
    ggplot(aes(reorder(Region, cases), cases)) +
    geom_bar(stat="identity", fill = "#0073b7", alpha = 0.6)     +
    coord_flip()                                    +
    # wrap axis.text for long country names like "Holy See (Vatican City State)"
    aes(reorder(stringr::str_wrap(Region, 20), 
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




#---------------------------------------------------------------------
#    - Charts 
#       - summiert, deaths und active Cases (Anzahl)
#       - Neuerkrankungen pro Tag (smoothed?)
#       - summiert, deaths und active Cases (Anzahl pro 100.000)
#       - Neuerkrankungen pro Tag pro 100.000 (smoothed?)
#---------------------------------------------------------------------
generateChartsWorld <- function(ma = -0.05){
  start.date <- as.Date("2020-02-01")
  ds.tmp <- df.world()               %>%
    dplyr::filter(day >= start.date)  
    
  norm.factor <- 100000/as.numeric(ds.tmp %>% summarize(population = max(population)))
  # World summiert
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
                        values = c("death"="grey70"),
                        labels = c("mortatlity")
    )                                
    
  
  # World summiert pro Tag
  ds.tmp <- df.input() %>% 
    group_by(day)                           %>%
    dplyr::filter(day >= start.date)        %>%
    summarize(
      cases.day     = sum(cases.day),
      active.day    = sum(active.day),
      recovered.day = sum(recovered.day),
      deaths.day    = sum(deaths.day),
      mortality.day = deaths.day/cases.day
    )          
  
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
#-------------------------------------------------------------
# CI an einem Stichtag, die top 20 Länder
#-------------------------------------------------------------

generateCountriesIncidents <- function(){
  title.text <- paste("Cumulated Incidences on ", today()-1)
  gg <- df.day()                                              %>% 
    mutate(Rsum = cases/population*100000)                    %>% 
    select(c(charcode, country.iso, Rsum, cases, population)) %>% 
    # Definition of plot
    top_n(20, Rsum)                                           %>%
    ggplot(aes(reorder(country.iso, Rsum), Rsum))   + 
    geom_col(fill = "#f8766d")     +
    coord_flip()                                    +
    # wrap axis.text for long country names like "Holy See (Vatican City State)"
    aes(reorder(stringr::str_wrap(country.iso, 20), Rsum), Rsum)              +
    # add numbers to the bars
    geom_text_repel(aes( y     = Rsum, 
                         label = format(round(Rsum,0), big.mark = ".", decimal.mark = ",")), 
                    hjust = "top",
                    direction = "x",
                    color = "black")                +
    labs( title = title.text,
          x = NULL, 
          y = "Anzahl Erkrankte pro 100.000 Einwohner"
    )                                               +
    theme(
      plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
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
    geom_col(fill = "grey50")     +
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
    geom_col(fill = "grey50")     +
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

