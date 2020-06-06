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


#-------------------------------
# helper fucntions
#-------------------------------
# Reproduktiosfaktor
rep.factor <- function(t, n_t, n_0) {
  return((n_t/n_0)^(1/t))
}
# Verdopplugszeit in Tagen
verdopplung <- function(t, n_t, n_0) {
  return(log(2)/log(rep.factor(t, n_t, n_0)))
}

#-------------------------------
# set options
#-------------------------------
# set encoding
options(encoding="UTF-8")


