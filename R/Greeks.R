## ---------------------------
##
## Script name: Greeks
## Purpose of script: calculate simple greeks from Black Scholes model, using derivmkts package
## Author: Meyrick Chapman
## Date Created: 2022-12-05
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
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(derivmkts)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------


S=100; K=100; sigma=0.2; r=0.05; T=1; d=0;

greeks(bscall(S, K, sigma, r, T, d), complete=FALSE, long=FALSE, initcaps=TRUE)

bsopt(S, K, sigma, r, T, d)

bsopt(S, c(90, 100, 110), sigma, r, T, d)

bsopt(S, c(90, 100, 110), sigma, r, T, d)[['Call']][c('Delta', 'Gamma'), ]


## plot Greeks for calls and puts for different stock prices

K <- 100; sigma <- 0.20; r <- 0.05; T <- 1; d <- 0

S <- seq(10, 200, by=5)

Call <- greeks(bscall(S, K, sigma, r, T, d))

Put <- greeks(bsput(S, K, sigma, r, T, d))

y <- list(Call=Call, Put=Put)

par(mfrow=c(4, 4), mar=c(2, 2, 2, 2))  ## create a 4x4 plot

for (i in names(y)) {
  
  for (j in rownames(y[[i]])) {  ## loop over greeks
    
    plot(S, y[[i]][j, ], main=paste(i, j), ylab=j, type='l')
    
  }
  
}
