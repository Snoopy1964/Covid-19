#########################################################################
#
# Download data from RapidAPI
#   API provided by api-sports
#   Documentation: https://rapidapi.com/api-sports/api/covid-193/details
#
# (1) load list of countries
# (2) initial load - only if there is no data
# (3) delta load by data
#
#
#
# ( ) clean up variables, tibbles, etc...
#
#########################################################################

# (0) set API key for rapid api
source("cov-dashboard//.api_key.R")

# (1) load functions
source("cov-dashboard//01_helper_func_data.R")


#----------------------------------------------
# (1) dowload list of countries
#----------------------------------------------
countries <- loadCountries("jhu")

#----------------------------------------------
# (2) initial load - only if there is no data
#----------------------------------------------
df <- loadData("jhu")

#----------------------------------------------
# ( ) clean up variables, tibbles, etc...
#----------------------------------------------
# rm(list = ls(pattern=".+\\.tmp"))
rm(list = ls(pattern="resp\\..+"))
rm(list = ls(pattern=".+\\.tmp"))
if(exists("country")) {rm("country")}

