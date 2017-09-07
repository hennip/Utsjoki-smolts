library(rjags)
library(runjags)
load.module("mix")
#library(xlsx)
library(tidyverse)
library(ggmcmc)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)


source("00-Functions/tidy-functions.r")
source("00-Functions/my-palette.r")


# Path for input data
pathIn<-"H:/Projects/ISAMA/data/der/input/Utsjoki-smolts/"
pathIn2<-"H:/Projects/ISAMA/data/orig/"

# Path for simulation output
pathOut<-"H:/Projects/ISAMA/prg/output/Utsjoki-smolts/"


source("01-Data/data-smolts-covariates.r")

