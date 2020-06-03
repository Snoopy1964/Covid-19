#these libraries need to be loaded
# library(utils)
# library(httr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"), 
    write_disk(fileName <- "./data/cases.csv", 
               overwrite = TRUE))

#read the Dataset sheet into “R”. The dataset will be called "data.raw".
data.raw <- read_csv("data/cases.csv", col_types = cols(dateRep = col_date(format = "%d/%m/%Y")))

# extract country data by geo codes
data.geoId <- data.raw %>%
  group_by(geoId)      %>%
  select(geoId, countriesAndTerritories, countryterritoryCode, popData2018) %>%
  distinct()

# prepare time seriese for cases grouped by geo codes
data.cases <- data.raw                          %>% # complete data with missing dates
  complete(
    dateRep = seq.Date(min(dateRep), max(dateRep), by="day"), 
    geoId
  )                                             %>% # cretae entries for missing dates
  arrange(geoId, dateRep)                       %>% # sort by country and reporting date
  mutate(
    cases.day = ifelse(is.na(cases), 0, cases), 
    deaths.day = ifelse(is.na(deaths), 0, deaths)
  )                                             %>% # fill missing numbers
  select(geoId, dateRep, cases.day, deaths.day)     # select parameters of interest
  

geoId.codes <- c(
  "DE",        # Germany
  "AT",        # Austria
  "ES",        # Spain
  "IT",        # Italy
  "FR",        # France
  
  # "IL",        # Israel
  "US"         # US
)

ds.world.ecdc <- data.cases        %>% 
  arrange(geoId, dateRep)     %>%
  group_by(geoId)             %>%
  left_join(data.geoId %>% select(geoId, popData2018)) %>% # join population data to calculate incidences
  mutate(
    cases.sum  = cumsum(cases.day),
    deaths.cum = cumsum(deaths.day),
    CI100k     = cases.day/popData2018*100000 #,
    # Year       = year(dateRep),
    # Month      = (year(dateRep) - 2020)*12 + month(dateRep),
    # Day        = as.numeric(dateRep - min(dateRep)) + 1,
    # Week       = trunc(Day/7) + 1
  )                            %>%
  
  select(
    geoId, 
    dateRep, 
    cases.day, 
    cases.sum, 
    deaths.day, 
    deaths.cum, 
    CI100k 
    # Day, 
    # Week, 
    # Month, 
    # Year
    ) 
  #  countriesAndTerritories, 
  #  popData2018)
  #  select(-c("day", "month", "year", "countryterritoryCode"))

ds.world.ecdc %>% 
  group_by(dateRep, Day)                  %>% 
  summarize(cases.day = sum(cases))       %>% 
  ungroup(dateRep, Day)                   %>%
  mutate(cases.total = cumsum(cases.day)) %>%
  ggplot(aes(x=dateRep)) +
  geom_step(aes(y=cases.total)) + 
  geom_smooth(aes(y=cases.total), span = 0.2)



ds.ecdc <- ds.world.ecdc %>% dplyr::filter(geoId %in% geoId.codes) %>% group_by(geoId)

gg.ds <- ds %>% ggplot(aes(x=dateRep))
gg.ds + geom_step(aes(y=CI100000)) + facet_wrap( ~ geoId)
