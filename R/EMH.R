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
install.packages("devtools")
install.packages("remotes")
library(remotes)
install_github("StuartGordonReid/emh")

library(tidyr)
library(plyr)
library(dplyr)
library(quantmod)
library(emh)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

plot_results <- function(results_df) {
  .plot_results_frequency(results_df)
  .plot_results_test_name(results_df)
}


.plot_results_frequency <- function(results_df) {
  frequencies <- unique(results_df$Frequency)
  pct_non_random <- c()
  for (fr in frequencies) {
    results_df_fr = results_df[results_df$Frequency == fr, ]
    pct_non_random <- c(pct_non_random,
                        sum(results_df_fr$Non_Random) /
                          length(results_df_fr$Non_Random))
  }
  names(pct_non_random) = frequencies
  par(las=2, mar=c(7.5,4.1,4.1,2.1), mgp=c(4, 1, 0))
  barplot(pct_non_random, main = "Percentage of tests with non-random result by frequency", cex.names=0.7)
}


.plot_results_test_name <- function(results_df) {
  test_names <- unique(results_df$Test_Name)
  pct_non_random <- c()
  for (fr in test_names) {
    results_df_fr = results_df[results_df$Test_Name == fr, ]
    pct_non_random <- c(pct_non_random,
                        sum(results_df_fr$Non_Random) /
                          length(results_df_fr$Non_Random))
  }
  names(pct_non_random) = test_names
  par(las=2, mar=c(7.5,4.1,4.1,2.1), mgp=c(4, 1, 0))
  barplot(pct_non_random, main = "Percentage of tests with non-random result by test name", cex.names=0.7)
}
## load up our functions into memory


## ---------------------------

startdate <- as.Date("2000-01-01")

SPY <- getSymbols('^GSPC', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)
AMZN<- getSymbols('AMZN', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)
VIX <- getSymbols('^VIX', src = 'yahoo',auto.assign = FALSE, warnings = FALSE, from =startdate)

SPYresults <- emh::is_random(SPY$GSPC.Close)
AMZNresults <- emh::is_random(AMZN$AMZN.Close)
VIXresults <- emh::is_random(VIX$VIX.Close)

emh::plot_results(SPYresults)
emh::plot_results(AMZNresults)
emh::plot_results(VIXresults)

View(SPYresults)
