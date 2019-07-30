# Description - Describe what the app does (e.g. visualizes births data)
# this section includes the non-reactive elements and everything used by both the
# UI and Server sides: functions, packages, data, etc.

############################.
##Packages ----
############################.
library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(stringr)
library(scales)
library(anytime)
library(plyr)
library("shinyWidgets")

###############################################.
## Functions ----

source("functions/stable.R")
###############################################.  

###############################################.
## Data ----

load("data/data.RDS")
###############################################.    

###############################################.
## Palettes ----
###############################################.   

## END
