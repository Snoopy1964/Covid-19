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
library("geojsonio")
library("wppExplorer")

cat("-------------------> helper functions\n")
print(environment())
message(print(getwd()))
if (exists("x.rapiapi.key")) {
  message(cat("API key used: ",x.rapidapi.key))
} else {
  
}



loadCountries <- function() {
  # load iso data for countries
  data("iso3166")
  population.tmp <- as.tibble(wpp.by.year(wpp.indicator("tpop"), 2020))
  
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
      as.tibble(countries.tmp)                           %>% 
      mutate(country.iso = convert.iso(value))           %>% 
      left_join(iso3166, by = c("country.iso" = "name")) %>% 
      dplyr::filter(!is.na(charcode))                    %>%
      rename(country = value)                            %>%
      left_join(population.tmp, by = "charcode")         %>%
      rename(population = value)                         %>%
      mutate(population = population*1000)
  )
}

loadData <- function() {
  
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
      df.all.tmp <- as.tibble(jsonlite::flatten(df.raw[5][1]$response))
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
      last = max(time), 
      cases.total = max(cases.total), 
      cases.critical=max(cases.critical), 
      cases.recovered=max(cases.recovered), 
      deaths.total=max(deaths.total), 
      tests.total = max(tests.total)
    )                                                 %>%
    left_join(countries, by=c("country" = "country")) %>%
    ungroup(country)                                  %>%
    select(c(charcode, day, last, country.iso, population, cases.total, cases.critical, cases.recovered, deaths.total, tests.total))
  
  
  return( df.tmp %>%
            group_by(charcode)  %>% 
            mutate(
              cases.active  = cases.total - cases.recovered,
              cases.day     = cases.total - dplyr::lag(cases.total, default=0),
              recovered.day = cases.recovered - dplyr::lag(cases.recovered, default=0),
              active.day    = cases.active - dplyr::lag(cases.active, default=0),
              tests.day     = tests.total - dplyr::lag(tests.total, default=NA),
              deaths.day    = deaths.total - dplyr::lag(deaths.total, default=0)
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



