## ---------------------------
##
## Script name: RealisedVol
## Purpose of script: compared realised vol using various measures, plus implied vol (VIX)
## Author: Meyrick Chapman
## Date Created: 2022-12-05
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   Close-to-Close Volatility (calc="close"):
# https://web.archive.org/web/20100421083157/http://www.sitmo.com/eq/172
# 
# OHLC Volatility: Garman Klass (calc="garman.klass"):
#   https://web.archive.org/web/20100326172550/http://www.sitmo.com/eq/402
# 
# High-Low Volatility: Parkinson (calc="parkinson"):
#   https://web.archive.org/web/20100328195855/http://www.sitmo.com/eq/173
# 
# OHLC Volatility: Rogers Satchell (calc="rogers.satchell"):
#   https://web.archive.org/web/20091002233833/http://www.sitmo.com/eq/414
# 
# OHLC Volatility: Garman Klass - Yang Zhang (calc="gk.yz"):
#   https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409
# 
# OHLC Volatility: Yang Zhang (calc="yang.zhang"):
#   https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409

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
library(TTR)
library(dygraphs)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
# Function to calculate monthly returns on a stock
daily_stock_returns <- function(ticker, start_year) {
  
  # Download the data from Yahoo finance
  symbol <- getSymbols(ticker, src = 'yahoo', 
                       auto.assign = FALSE, warnings = FALSE)
  
  # Tranform it to monthly returns using quantmode::periodReturn
  data <- periodReturn(symbol, period = 'daily', 
                       subset=paste(start_year, "::", sep = ""),
                       type = 'log') 
  
  # Let's rename the column of returns to something intuitive because
  # the column name is what will eventually be displayed
  colnames(data) <- as.character(ticker)
  
  # We want to be able to work with the xts objects so let's explicitly
  # assign them into the global environment using ticker name 
  assign(ticker, data, .GlobalEnv)
}
## ---------------------------

startdate <- as.Date("2020-01-01")

VIX <- getSymbols('^VIX', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)
SPY <- getSymbols('^GSPC', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)

ohlc <- SPY[,c("GSPC.Open","GSPC.High","GSPC.Low","GSPC.Close")]
vClose <- volatility(ohlc, calc="close")
vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
vGK <- volatility(ohlc, calc="garman")
vPK <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
vYZ <- volatility(ohlc,calc="yang.zhang")

my_data = cbind(vClose, vGK, vYZ, vPK)
colnames(my_data) = c("Close-to-Close", "Garman-Klass", "Yang Zhang", "Parkinson")

dygraph(my_data, main = "Realised volatility: S&P500") %>%
  dyAxis("y", label = "% annualised volatility") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"))

merged_vol <- merge.xts(VIX$VIX.Close/100, my_data)

dygraph(merged_vol, main = "Realised volatility (S&P500) & VIX") %>%
  dyAxis("y", label = "% annualised volatility") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"))

lng_merged_vol <- 
  fortify(merged_vol) %>%
  pivot_longer(c(-Index), names_to = 'key', values_to = 'value')


p1<- ggplot(merged_vol, aes(x = Index)) +
geom_line(aes(y = `VIX.Close`, color = "VIX closing level"))+
geom_line(aes(y = Close.to.Close, color = "Realised vol: close-to-close"))+
geom_line(aes(y = Garman.Klass, color = "Realised vol: Garman-Klass"))+
geom_line(aes(y = Yang.Zhang, color = "Realised vol: Yang Zhang"))+
geom_line(aes(y = Parkinson, color = "Realised vol: Parkinson"))+
scale_y_continuous(name = 'Annualised volatility', labels = scales::percent)+
scale_color_manual(values=c("red", "blue", "green", "brown", "grey")) +
labs(title = "Realised volatility of S&P500 and VIX market implied volatility",
color = "", y = "Annualised volatility", x = "Date")
p1

