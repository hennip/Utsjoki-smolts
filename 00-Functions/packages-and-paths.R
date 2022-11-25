library(rjags)
library(runjags)
load.module("mix")
#library(xlsx)
library(tidyverse)
library(ggmcmc)
library(readxl)
#library(xlsx)
library(forcats)
library(lubridate)
library(stringr)
require(gridExtra)



source("00-Functions/tidy-functions.r")
source("00-Functions/my-palette.r")
source("00-Functions/smwrg_m.r")
source("00-Functions/sustemp.r")
source("00-Functions/rollsum.r")
#source("00-Functions/smolts-data-to-jags.r")
source("00-Functions/s-dat-jags_AR.r")

pathMain<-readRDS("C:/Temp/path-main.rds")

# Path for input data
#pathIn<-paste0(pathMain,"05-Vanha_O/Projects/ISAMA/data/der/input/Utsjoki-smolts/")
pathIn<-paste0(pathMain,"Teno_shared/data/orig/")
#pathIn2<-paste0(pathMain,"05-Vanha_O/Projects/ISAMA/data/orig/")

# Path for simulation output
pathOut<-paste0(pathMain,"output/Utsjoki-smolts/")


#source("01-Data/data-smolts-covariates.r")
#source("01-Data/data-simul.r")

