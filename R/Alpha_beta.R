## ---------------------------
##
## Script name: Alpha_Beta
## Purpose of script: Alpha_Beta calculation for portfolio of stocks
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
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
library(ggplot2)
library(dygraphs)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
source("R/ggstdplots.R")

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

year <- 2007

monthly_stock_returns('GOOG', year)
monthly_stock_returns('JPM', year)
monthly_stock_returns('AMZN', year)
monthly_stock_returns('SPY', year)

# Merge the 3 monthly return xts objects into 1 xts object.
merged_returns <- merge.xts(GOOG, JPM, AMZN)

w <- c(.25, .25, .50)

# Now use the built in PerformanceAnalytics function Return.portfolio
# to calculate the monthly returns on the portfolio,
portfolio_monthly_returns <- Return.portfolio(merged_returns, weights = w)

# obtain and calculate the benchmark returns
benchmark_monthly_returns <- SPY

#Get sum of NA per column
colSums(is.na(merged_returns))

#Plot
dygraph(merged_returns, main = "Monthly returns: Google v JP Morgan v Amazon") %>%
  dyAxis("y", label = "% monthly returns") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

# Now use the built in PerformanceAnalytics function Return.portfolio
# to calculate the monthly returns on the portfolio,
portfolio_monthly_returns <- Return.portfolio(merged_returns, weights = w)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(portfolio_monthly_returns, main = "Portfolio Monthly Return") %>%
  dyAxis("y", label = "%")

#par(bg = "blue") # Color
p <- gg.charts.PerformanceSummary2(portfolio_monthly_returns, main="Portfolio performance", xaxis_date_breaks = "3 years")

#Calculate Metrics 
CAPM.beta(portfolio_monthly_returns, benchmark_monthly_returns, .035/12)
CAPM.beta.bull(portfolio_monthly_returns, benchmark_monthly_returns, .035/12)
CAPM.beta.bear(portfolio_monthly_returns, benchmark_monthly_returns, .035/12)

CAPM.alpha(portfolio_monthly_returns, benchmark_monthly_returns, .035/12)
CAPM.jensenAlpha(portfolio_monthly_returns, benchmark_monthly_returns, .035/12)

