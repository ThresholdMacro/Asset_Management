## ---------------------------
##
## Script name: Sharpe_ratio
## Purpose of script: calculate Sharpe ratio of a selected portfolio
## Author: Meyrick Chapman
## Date Created: 2022-11-25
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
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# Function to calculate monthly returns on a stock
monthly_stock_returns <- function(ticker, start_year) {
  
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
## ---------------------------

# Choose the starting year and assign it to the 'year' variable
year <- 2000

# Use the function the monthly returns on 3 stocks, and pass in the 'year'
# value. Let's choose Google, JP Morgan and Amazon
monthly_stock_returns('GOOG', year)
monthly_stock_returns('JPM', year)
monthly_stock_returns('AMZN', year)

# Merge the 3 monthly return xts objects into 1 xts object.
merged_returns <- merge.xts(GOOG, JPM, AMZN)

# Before we combine these into a portfolio, graph the individual returns
# and see if anything jumps out as unusual. It looks like something 
# affected Google in March of 2014, but didn't affect JP Morgan or Amazon.
dygraph(merged_returns, main = "Monthly returns: Google v JP Morgan v Amazon") %>%
  dyAxis("y", label = "% monthly returns") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  
  # We have the 3 monthly returns saved in 1 object.
  # Now, let's choose the respective weights of those 3.
  # Here we'll allocate 25% to Google, 25% to JP Morgan and 50% to Amazon.
  w <- c(.25, .25, .50)
  
  # Now use the built in PerformanceAnalytics function Return.portfolio
  # to calculate the monthly returns on the portfolio,
  portfolio_monthly_returns <- Return.portfolio(merged_returns, weights = w)
  
  # Use dygraphs to chart the portfolio monthly returns.
  dygraph(portfolio_monthly_returns, main = "Portfolio Monthly Return") %>%
    dyAxis("y", label = "%")
  
  # Add the wealth.index = TRUE argument and, instead of monthly returns,
  # the function will return the growth of $1 invested in the portfolio.
  dollar_growth <- Return.portfolio(merged_returns, weights = w, 
                                    wealth.index = TRUE)
  
  # Use dygraphs to chart the growth of $1 in the portfolio.
  dygraph(dollar_growth, main = "Growth of $1 Invested in Portfolio") %>%
    dyAxis("y", label = "$")

  # Method 1: use the Return.excess function from PerformanceAnalytics,
  # then calculate the Sharpe Ratio manually.
  portfolio_excess_returns <- Return.excess(portfolio_monthly_returns, 
                                            Rf = .0003)
  
  sharpe_ratio_manual <- round(
    mean(portfolio_excess_returns) / StdDev(portfolio_excess_returns), 4
  )
  
  print(sharpe_ratio_manual)
  
  # If we wanted to use the original, 1966 formulation of the Sharpe Ratio,
  # there is one small change to the code in Method 1
  sharpe_ratio <- round(
    SharpeRatio(portfolio_monthly_returns, Rf = .0003), 4
  )

print(sharpe_ratio)
  