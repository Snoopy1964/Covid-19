#########################################################################
#
# Download data from RapidAPI
#   API provided by api-sports
#   Documentation: https://rapidapi.com/api-sports/api/covid-193/details
#
#   !!!!!!!!!!!!!!!!!!!!!!!!!! Attention !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!! please sign-up for a free API key at https://rapidapi.com/  !!!
#   !!!   create a file .api_key.R in the root directory and        !!!
#   !!!   set a variable x.rapidapi.key                             !!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!! Attention !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# (1) load list of countries
# (2) load data 
# (3) delta load by data
#
#
#
# ( ) clean up variables, tibbles, etc...
#
#########################################################################

library("utils")
library("httr")
library("jsonlite")

library("lubridate")
library("tidyverse")
library("readxl")
library("ggmap")
# library("ggplotgui"
library("ggrepel")
#library("geojsonio")
library("wppExplorer")

cat("------> helper functions\n")
if (exists("x.rapiapi.key")) {
  message(cat("API key used: ",x.rapidapi.key))
} else {
  
}
#------------------------------
# setting some globaloptions()
#------------------------------
options(dplyr.summarise.inform = FALSE) # get rid of annoying messages from summarise()
options("Debug.Dashboard" = TRUE)       # enable app specific debugging

loadCountries <- function(source = "jhu") {
  # load iso data for countries
  data("iso3166")
  population.tmp <- as.tibble(wpp.by.year(wpp.indicator("tpop"), 2020))
  
  # load countries from RAPID API  
  if(source == "rapidapi") {
    # dowload list of countries
    resp.tmp <- GET("https://covid-193.p.rapidapi.com/countries", 
                    add_headers('x-rapidapi-host' = 'covid-193.p.rapidapi.com',
                                'x-rapidapi-key'  = x.rapidapi.key)
                    # query = list(country = "Germany")
    )
    df.raw.tmp    <- fromJSON(content(resp.tmp, "text", encoding="UTF-8"))
    countries.tmp <- df.raw.tmp[5][1]$response 
    
    return(
      countries.tmp                           %>% 
        as_tibble(countries.tmp)                           %>% 
        mutate(country.iso = convert.iso(value))           %>% 
        left_join(iso3166, by = c("country.iso" = "name")) %>% 
        dplyr::filter(!is.na(charcode))                    %>%
        rename(country = value)                            %>%
        left_join(population.tmp, by = "charcode")         %>%
        rename(population = value)                         %>%
        mutate(population = population*1000)
    )
  } else if(source == "jhu") {
    jhu_countries <- as_tibble(
      data.table::fread(
        "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv", 
        na.string = "---") # Namibia has iso2 = NA :-(
    )                                                       %>% 
      # remove cruise ships -> they don't have an iso2 code
      dplyr::filter(iso2 != "")                             %>%
      
      # use variable names from iso366 package
      # !!!! Attention !!!!
      # avoid iso2 and iso3 clash for Kosovo, 
      # jhu data set has mapped it to XK/XKS but 
      # iso3166 data used XK/XKW for region Western Asia
      # XKX is temporary iso3 code -> KX is not used
      mutate(
        charcode  = if_else(iso2 == "XK" , "KX" , iso2),
        charcode3 = if_else(iso3 == "XKS", "XKX", iso3)
      )                                                     %>%
      left_join(iso3166, by = "charcode")                   %>%
      # remove "Unkonwn Province_States
      dplyr::filter(Province_State != "Unknown")            %>%
      # correct column is.country 
      # Condition: Province_State is not empty 
      mutate(is.country = if_else(UID < 1000, TRUE, FALSE)) %>%
      # select relevant columns
      select(Combined_Key, name, charcode, charcode3.x, UID, uncode, Population, is.country) %>% 
      # rename to output format
      rename(
        country     = Combined_Key, 
        country.iso = name,
        charcode3   = charcode3.x,
        population  = Population
      )
      
    return(jhu_countries %>% dplyr::filter(is.country))
    
  } else {
    stop(paste("unkonowd data source: ",source))
  }
  
}

loadData      <- function(source = "jhu") {
  if(source == "rapidAPI") {
    return(loadData.rapidAPI())
  } else if(source == "jhu") {
    return(loadData.jhu())
  } else {
    stop(paste("unknown data source: ", source))
  }
}

loadData.jhu <- function() {
  # load latest Covid-2019 data: confirmed cases
  jhu_cases <- as_tibble(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  cases.check.sum <- sum(jhu_cases[[ncol(jhu_cases)]]) # check sum of all cases 
  cases     <- jhu_cases %>% 
    pivot_longer(names(jhu_cases)[-(1:4)], names_to = "day", values_to = "cases") %>%
    # Lat/Long is not needed, furthermore 
    #it slightly differs for Cameroon, South Sudan which causes issues, when joining
    select(-c("Lat", "Long"))
  
  # load latest Covid-2019 data: deaths
  jhu_deaths <- as_tibble(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
  deaths.check.sum <- sum(jhu_deaths[[ncol(jhu_deaths)]]) # check sum of all deaths 
  deaths     <- jhu_deaths %>% 
    pivot_longer(names(jhu_deaths)[-(1:4)], names_to = "day", values_to = "deaths") %>%
    # Lat/Long is not needed, furthermore 
    #it slightly differs for Cameroon, South Sudan which causes issues, when joining
    select(-c("Lat", "Long"))
  
  
  # load latest Covid-2019 data: recovered
  jhu_rec <- as_tibble(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
  rec.check.sum <- sum(jhu_rec[[ncol(jhu_rec)]]) # check sum of all recovered 
  recovered     <- jhu_rec %>% 
    pivot_longer(names(jhu_rec)[-(1:4)], names_to = "day", values_to = "recovered") %>%
    # Lat/Long is not needed, furthermore 
    #it slightly differs for Cameroon, South Sudan which causes issues, when joining
    select(-c("Lat", "Long"))
  
  
  # join data
  df.jhu <- cases %>% left_join(deaths) %>% left_join(recovered) %>%
    # select(-c("Lat", "Long"))                                    %>%
    mutate(day       = mdy(day),
           recovered = if_else(is.na(recovered), as.integer(0), recovered),
           active    = cases - deaths - recovered)               %>%
    rename(country = `Country/Region`)                           %>%
    rename(state   = `Province/State`)                           %>%
    
# For the following countries the numbers are not available on country level, 
# but only on Province/State level 
#
#   `Country/Region`      nr
#   <chr>              <int>
#   1 Australia         1088
#   2 Canada            1904
#   3 China             4488
#   4 Denmark            408
#   5 France            1496
#   6 Netherlands        680
#   7 United Kingdom    1496 
#
# -> aggregate the numbers per day and Country/Region -> Province/State is not supported
    group_by( country, day)                                      %>% 
    summarize(cases     = sum(cases), 
              active    = sum(active),
              deaths    = sum(deaths), 
              recovered = sum(recovered)
              )                                                  %>%
    # join with country data only
    left_join(countries, by = "country")                         %>%
    # select the relevant columns (same definition as of rapidAPI
    select(c(charcode, day, country.iso, population, cases, active, recovered, deaths))
  
  return(df.jhu           %>%
           mutate(
             cases.day     = cases      - dplyr::lag(cases,      default=0),
             active.day    = active     - dplyr::lag(active,     default=0),
             recovered.day = recovered  - dplyr::lag(recovered,  default=0),
             deaths.day    = deaths     - dplyr::lag(deaths,     default=0)
           )              %>% 
           ungroup(country)
  )
}
  
loadData.rapidAPI <- function() {
  
  #----------------------------------------------
  # (2) initial load - only if there is no data
  #----------------------------------------------
  # for (country in countries) {
  for (i in 1:length(countries$country)) {
    country <- countries$country[i]
    cat(paste("loading data for:",country, "......."))
    resp.tmp <- GET("https://covid-193.p.rapidapi.com/history", 
                    add_headers('x-rapidapi-host' = 'covid-193.p.rapidapi.com',
                                'x-rapidapi-key'  = x.rapidapi.key),
                    query = list(country = country)
                    # query = list(country = "Germany")
    )
    df.raw <- fromJSON(content(resp.tmp, "text", encoding="UTF-8"))
    if (i==1) {
      df.all.tmp <- as_tibble(jsonlite::flatten(df.raw[5][1]$response))
    } else {
      df.all.tmp <- rbind(df.all.tmp, as.tibble(jsonlite::flatten(df.raw[5][1]$response)))
    }
    ## assign(paste("df.", country, ".tmp", sep=""), as.tibble(flatten(df.raw[5][1]$response)))
    cat(" done\n")
  }
  
  df.all.tmp <- df.all.tmp %>% mutate(
    day = ymd(day),
    time = ymd_hms(time),
    cases.new = as.integer(cases.new),
    deaths.new = as.integer(deaths.new)
  )

  df.tmp <- df.all.tmp %>% 
    # dplyr::filter(country=="Germany") %>%
    group_by(country, day) %>%
    arrange(country,time)   %>%
    select(c(country, day, time, cases.critical, cases.recovered, cases.total, deaths.total, tests.total)) %>%
    summarize(
      last      = max(time), 
      cases     = max(cases.total), 
      critical  = max(cases.critical), 
      recovered = max(cases.recovered), 
      deaths    = max(deaths.total), 
      tests     = max(tests.total)
    )                                                 %>%
    left_join(countries, by=c("country" = "country")) %>%
    ungroup(country)                                  %>%
    select(c(charcode, day, last, country.iso, population, cases, recovered, deaths, tests))
  
  
  return( df.tmp %>%
            mutate(
              active        = cases      - recovered - deaths,
              cases.day     = cases      - dplyr::lag(cases,      default=0),
              recovered.day = recovered  - dplyr::lag(recovered,  default=0),
              active.day    = active     - dplyr::lag(active,     default=0),
              # tests.day     = tests      - dplyr::lag(tests,      default=NA),
              deaths.day    = deaths     - dplyr::lag(deaths,     default=0)
            )
  )
}

#------------------------------------------------------------------------------
# (1) convert parameter country from covid-19 API to standard names of iso code
#------------------------------------------------------------------------------
# 
# (1) convert parameter country from covid-19 API to standard names of iso code
convert.iso <- function(names) {
  
  country.iso <- vector()
  for (s in names) {
    # cat(paste("transforming...: ",s))
    s <- str_replace_all(s, "-", " ")
    if (s == "Bolivia") {s = "Bolivia, Plurinational State Of"}
    if (s == "British Virgin Islands") {s = "Virgin Islands, British"}
    if (s == "Brunei") {s = "Brunei Darussalam"}
    if (s == "Cabo Verde") {s = "Cape Verde"}
    if (s == "CAR") {s = "Central African Republic"}
    if (s == "Cura&ccedil;ao") {s = "Curacao"}
    if (s == "DRC") {s = "Congo, The Democratic Republic Of The"}
    if (s == "Eswatini") {s = "Swaziland"}
    if (s == "Faeroe Islands") {s = "Faroe Islands"}
    if (s == "Falkland Islands") {s = "Falkland Islands  (Malvinas)"}
    if (s == "Guinea Bissau") {s = "Guinea-Bissau"}
    if (s == "Iran") {s = "Iran, Islamic Republic Of"}
    if (s == "Moldova") {s = "Moldova, Republic of"}
    if (s == "Laos") {s = "Lao Peoples Democratic Republic"}
    if (s == "Libya") {s = "Libyan Arab Jamahiriya"}
    if (s == "North Macedonia") {s = "Macedonia, the Former Yugoslav Republic Of"}
    if (s == "Palestine") {s = "Palestinian Territory, Occupied"}
    if (s == "R&eacute;union") {s = "Reunion"}
    if (s == "Russia") {s = "Russian Federation"}
    if (s == "S Korea") {s = "Korea, Republic of"}
    if (s == "Saint Kitts and Nevis") {s = "Saint Kitts And Nevis"}
    if (s == "Saint Pierre Miquelon") {s = "Saint Pierre And Miquelon"}
    if (s == "Sint Maarten") {s = "Sint Maarten (Dutch part)"}
    if (s == "St Vincent Grenadines") {s = "Saint Vincent And The Grenedines"}
    if (s == "St Barth") {s = "Saint Barthelemy"}
    if (s == "Syria") {s = "Syrian Arab Republic"}
    if (s == "Taiwan") {s = "Taiwan, Province Of China"}
    if (s == "Tanzania") {s = "Tanzania, United Republic of"}
    if (s == "Timor Leste") {s = "Timor-Leste"}
    if (s == "Turks and Caicos") {s = "Turks and Caicos Islands"}
    if (s == "UAE") {s = "United Arab Emirates"}
    if (s == "UK") {s = "United Kingdom"}
    if (s == "US Virgin Islands") {s = "Virgin Islands, U.S."}
    if (s == "USA") {s = "United States"}
    if (s == "Vatican City") {s = "Holy See (Vatican City State)"}
    if (s == "Venezuela") {s = "Venezuela, Bolivarian Republic of"}
    if (s == "Vietnam") {s = "Viet Nam"}
    if (s == "Ivory Coast") {s = "Cote DIvoire"}
    # if (s == "") {s = ""}
    # if (s == "") {s = ""}
    # if (s == "") {s = ""}
    # cat(paste("...... to: \t\t",s, "\n"))
    country.iso <- c(country.iso, s)
  }
  
  return(country.iso)
}



