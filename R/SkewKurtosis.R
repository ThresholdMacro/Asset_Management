## ---------------------------
##
## Script name: SkewKurtosis
## Purpose of script: calculated skew and kurtosis of a market
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
library(quantmod)
#library(moments)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
## load up our functions into memory
# Function to calculate monthly returns on a stock
daily_stock_returns <- function(ticker, start_year) {
  
  # Download the data from Yahoo finance
  symbol <- getSymbols(ticker, src = 'yahoo', 
                       auto.assign = FALSE, warnings = FALSE)
  
  # Tranform it to monthly returns using quantmode::periodReturn
  data <- periodReturn(symbol, period = 'monthly', 
                       subset=paste(start_year, "::", sep = ""),
                       type = 'log') 
  
  # Let's rename the column of returns to something intuitive because
  # the column name is what will eventually be displayed
  colnames(data) <- as.character(ticker)
  
  # We want to be able to work with the xts objects so let's explicitly
  # assign them into the global environment using ticker name 
  assign(ticker, data, .GlobalEnv)
}
#skewness
skewness <- function(x){
  skew <- sum((x-mean(x))^3)/((length(x)-1)*sd(x)^3)
  return(skew)
}
#kurtosis
kurtosis <- function(x){
  kurt <- sum((x-mean(x))^4)/((length(x)-1)*sd(x)^4)
  return(kurt)
}

## ---------------------------

# try this with other stocks - for instance, AMZN, JPM, GOOG
startyear <- 2000
SPY <- daily_stock_returns('^GSPC',startyear)

print(skewness(SPY))

print(kurtosis(SPY))
par(mfrow=c(1,1))
g = SPY
m<-mean(g)
std<-sqrt(var(g))
hist(
  SPY,
  breaks = 100,
  main = paste0(
    'S&P500 skew ', round(skewness(SPY),1),': kurtosis ',
    round(kurtosis(SPY), 1),
    ': relative to a normal distribution'
  ),
  xlab=paste0('Monthly returns since ',startyear),
  col = "lightblue",
  border = "grey"
)
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
