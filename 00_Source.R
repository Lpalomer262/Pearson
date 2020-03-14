# HEADER ------------------------------------------------------------------
#' PROGRAM NAME: SOURCE.R
#' PROJECTS: PEARSON
#' DESCRIPTIONS: SOURCE
#' 
#' 
#' PROGRAMMER: LEONARDO PALOMERA
#' DATE: 3/9/2020
#' R VERSION: 3.5.0
#' INPUT FILES
#' OUTPUT FILES
#' SECTION

rm(list = ls())
gc(reset = TRUE)

# SOURCE & PACKAGE IMPORT -------------------------------------------------
#' INSTALL PACKAGES
#' install.packages("tidyr")
#' install.packages("dplyr")
#' install.packages("readr")
#' install.packages("ggplot2")
#' install.packages("stringr")
#' install.packages("purrr")
#' install.packages("googlesheets")
#' install.packages("lubeidate")
#' install.packages("ggmap")
#' install.packages("sf)
#' install.packages("mapview")
#' install.packages("httr")
#' install.packages("rjson")
#' install.packages("leaflet")
#' install.packages("sqldf")
#' install.packages("gtable")
#' install.packages("ggalluvial")

#' LOAD LIBRARIES
library(tidyr)
library(tibble)
library(forcats)
library(readr)
library(ggplot2)
library(stringr)
library(purrr)
library(googlesheets)
library(lubridate)
#library(ggmap)
#library(geosphere) #' DISTM FUNCTION
#library(sf)
#library(mapview)
library(tidyverse)
library(knitr)
library(scales)
library(sqldf)
library(gtable)
library(ggalluvial)
library(dplyr)

#' STRAVA LIBRARIES
#library(httr)
#library(rjson)

#' library(rJava) #' NEED HELP LOADING RJAVA
#' library(OpenStreetMap) #' NEEDS RJAVA TO LOAD
#library(leaflet)
