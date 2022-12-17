## ---------------------------
##
## Script name: EMH
## Purpose of script: demostrate  the (In)Efficient Market Hypothesis on real market data
## Author: Meyrick Chapman
## Date Created: 2022-12-17
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(plyr)
library(dplyr)
library(quantmod)
library(emh)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

startdate <- as.Date("2000-01-01")

SPY <- getSymbols('^GSPC', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)
AMZN<- getSymbols('AMZN', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)
VIX <- getSymbols('^VIX', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)

SPYresults <- emh::is_random(SPY)
AMZNresults <- emh::is_random(AMZN)
VIXresults <- emh::is_random(VIX)

emh::plot_results(SPYresults)
emh::plot_results(AMZNresults)
emh::plot_results(VIXresults)
