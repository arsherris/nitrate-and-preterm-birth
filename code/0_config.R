## SETUP
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# define options for project

  # define exposure of interest
  # options: "Arsenic", "Nitrate", "Perchlorate"
  # potential options for future research: "Uranium", "Chromium (VI)", "Gross Alpha","Selenium",
  # "Mercury", "Iron", "Manganese", "Vanadium","Chromium (Total)", "Fluoride", "Mercury"

study_exposure <- "Nitrate"

# define outcome of interest

early_ptb      <- "prem_20_to_31"
late_ptb       <- "prem_32_to_36"
study_outcomes <- c("prem_20_to_31", "prem_32_to_36")


# load required packages

library(readr)
library(zoo)
library(foreign)
library(sf)
library(RColorBrewer)
library(lubridate)
library(data.table)
library(lme4)
library(survival)
library(zoo)
library(broom)
library(tidyverse)


# define coordinate reference systems

# geographic CRS (for plotting points)
crs_geo <- st_crs("+init=epsg:4269 +proj=longlat
                      +ellps=GRS80 +datum=NAD83
                      +no_defs +towgs84=0,0,0")  # NAD83, geographic data

# projected CRS (for creating buffer)
crs_projected <- st_crs("+proj=utm +zone=11 +datum=WGS84")

# define universal function

# function to write csv output with specified folder, name, and current date
  # input: object name and folder (ending in foward slash)
  # output: saved cvs file in format: "studyexposure_objectname_date.csv" in specified folder
  # note: can be substituted with use of here::here() function

write_output <- function(x, folder) {
  write_csv(x = x, path = paste(
    folder, 
    study_exposure, "_",
    deparse(substitute(x)), "_", 
    Sys.Date(), ".csv", sep = ""
  )
  )
}


# end


