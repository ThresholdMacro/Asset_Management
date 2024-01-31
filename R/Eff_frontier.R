## ---------------------------
##
## Script name: Eff_frontier
## Purpose of script: calculate and display a toy efficient frontier
## Author: Meyrick Chapman
## Date Created: 2022-11-25
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   https://www.youtube.com/watch?v=Y-rLyl4rrXw
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
require(ggplot2)
library(ggplot2)
library(ggthemes)
library(readr)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
## ---------------------------

# mearn return of stocks
mu_a <- 5
mu_b <- 7

# risk of the stocks
sigma_a <- 3
sigma_b <- 4

# correlation between stocks, negative
rho <- -0.4

# scale of amount to invest in stock 'a' 
w <- seq(from=0,to=1, by = 0.02)
# weighted average return of investment in stock 'a' minus return investment in stock 'b'

ret <- w * mu_a + (1-w) * mu_b
vol <-
  sqrt(w ^ 2 * sigma_a ^ 2 + (1 - w) ^ 2 * sigma_b ^ 2 + 2 * w * (1 - w) *
         rho * sigma_a * sigma_b)

data <- data.frame(ret=ret, vol = vol)
  
p <- ggplot(data, aes(x=vol, y=ret))+
  geom_point()+
  theme_bw()+
  ggtitle("Efficient frontier") +
  xlab("Volatility")+ylab("Return")
  
p
p + geom_text(data=data, aes(x=vol, y= ret+0.10, label= w*100 ))

# choose a portfolio with certain characteristics. 
p <- p + annotate("point", x=2.5, y= 6, colour='blue', size = 3)

# can we do better? Any point within the curve can be improved by tracking up to dot above 
# that will give the optimal risk/return characteristics
# alternatively, shift left to get the same return for lower volatility
p<- p + geom_segment(aes(x=2.5, xend=2.5, y=6, yend=6.3), 
                 arrow = arrow(length = unit(0.5, "cm")), colour='red')
p <-p + geom_hline(yintercept = 6, colour='red', linetype='dashed')
p <- p + annotate("rect", xmin = 1.75, xmax = 4, ymin = 5, ymax = 6, 
                  alpha = 0.3)

p
