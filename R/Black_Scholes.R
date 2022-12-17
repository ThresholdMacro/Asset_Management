## ---------------------------
##
## Script name: Black_Scholes
## Purpose of script: simple Black Scholes model demonstrating sensitivities and constituents
## Author: Meyrick Chapman
## Date Created: 2022-11-27
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
library(derivmkts)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory


Black_Scholes <- function(S=100, K=100, r=0.05, T= 1.00, sigma=0.33, type="C"){
#Formula: C = e-rt[ UN(d1)-EN(d2)]
  if(type=="C"){
    d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)
    #call option delta based on normal distribution of returns and expected volatility
    #less discounted forward strike * probability that the call option will be exercised at expiration
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
    d2 <- d1 - sigma*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}


## ---------------------------

year <- 2007

# Estimate recent realised volatility
# Get the Close column(4) from the WIKI dataset from Quandl of the APPL stock 
symbol <- getSymbols('AAPL', src = 'yahoo', 
                     auto.assign = FALSE, warnings = FALSE)
# Tranform to daily returns using quantmode::periodReturn
data <- periodReturn(symbol, period = 'daily', 
                     subset=paste(year, "::", sep = ""),
                     type = 'log') 

# Retrieve the first 50 values

recent_data <- round(tail(data,50),3)


# Calculate the standard deviation of the log returns

Standard_deviation <- sd(recent_data$daily.returns,na.rm=TRUE)
annual_sigma <- Standard_deviation * sqrt(250)

print(annual_sigma)

# Second approach (by far the most common) is to estimate the implied volatility from the option price itself
S <- 100
K <- 100
C <- 24.00 #(Actual Call Price on 27 November 2022)
r <- 0.05
# Options expire May 19, 2023
T <- 1.00
sigma1 <- seq(0.01,0.5,0.005)

# The variable CallValueperSigma would store all the option prices that are 
# calculated with the different values of sigma along the sigma vector. 

CallValueperSigma <- Black_Scholes(S,K,r,T,sigma1,"C") 

# IV is the the min value of the difference between each value of the  #CallValueperSigma  vector and the call price. 

IV <- which.min(abs(CallValueperSigma-C))/2 

#IV is divided by two in order to get the specific Implied Volatility value from the sigma vector, because sigma starts from 0 to 0.5.


#Section II - impact of stock price less strike, ignoring time and volatility
#Example S-K (i.e. BS which ignores volatility and time value of money)
# create a vector of underlying prices 
pr <- seq(from=100*0.75, to=100*1.25, by=0.1)
#apply those underlying prices to the Black_Scholes function, at expiry
cv <- Black_Scholes(S = pr,T=0)
pv <- Black_Scholes(S = pr,T=0,type="P")

p2<- ggplot(data.frame(pr, cv, pv), aes(x = pr)) +
  geom_line(aes(y = cv, color = "Call")) +
  geom_line(aes(y = pv, color = "Put")) +
  scale_color_manual(values=c("red", "blue")) +
  labs(title = "Option Price Sensitivity to Price - at expiry (i.e. removes vol and time)", 
       color = "", y = "Option Price", x = "Stock Price")
p2+ annotate("text", x = 100, y = 27,
             label = "S=variable, K=100, r=0.05, T=0, sigma=0.33",
             color = "black", size = 3.5)

#Section III - impact of vol and time

# create a vector of ascending time to expiry
tv <- seq(from=0.01, to=1, by=0.01)
#apply those expiry periods to the Black_Scholes function
cv <- Black_Scholes(T = tv)
pv <- Black_Scholes(T = tv,type="P")

p1<- ggplot(data.frame(tv, cv, pv), aes(x = tv)) +
  geom_line(aes(y = cv, color = "Call")) +
  geom_line(aes(y = pv, color = "Put")) +
  scale_color_manual(values=c("red", "blue")) +
  labs(title = "Option Price Sensitivity to Time", 
       color = "", y = "Option Price", x = "Time to Expiry in Years")
p1+ annotate("text", x = 0.5, y = 16,
            label = "S=100, K=100, r=0.05, T=variable, sigma=0.33",
            color = "black", size = 3.5) + 
    annotate("text", x = 1.00, y = 12.5,
           label = "Risk-free rate",
           color = "black", size = 4,hjust = 1)+
  geom_segment(aes(x = 1.00, y = 10.5, xend = 1.00, yend = 15),
               arrow = arrow(length = unit(0.15, "cm")))

# create a vector of ascending implied volatilities
vv <- seq(from=0.01, to=0.50, by=0.01)
#apply those volatilities to the Black_Scholes function
cv <- Black_Scholes(sigma = vv)
pv <- Black_Scholes(sigma = vv,type="P")

p<- ggplot(data.frame(vv, cv, pv), aes(x = vv)) +
  geom_line(aes(y = cv, color = "Call")) +
  geom_line(aes(y = pv, color = "Put")) +
  scale_color_manual(values=c("red", "blue")) +
  labs(title = "Option Price Sensitivity to Volatility", 
       color = "", y = "Option Price", x = "Volatility")
p+ annotate("text", x = 0.25, y = 22,
            label = "S=100, K=100, r=0.05, T=1.0, sigma=variable",
            color = "black", size = 3.5) +
  annotate("text", x = 0.3, y = 2.8,
           label = "Risk-free rate",
           color = "black", size = 4)+
  geom_segment(aes(x = 0.15, y = 2.8, xend = 0.05, yend = 2.8),
               arrow = arrow(length = unit(0.15, "cm")))

#Section IV - complete pricing
# create a vector of underlying prices 
pr <- seq(from=100*0.75, to=100*1.25, by=0.1)
#apply those underlying prices to the Black_Scholes function, at default time-to-expiry (1 year)
cv <- Black_Scholes(S = pr, T=1)
pv <- Black_Scholes(S = pr,T=1,type="P")

rf <- K-(pr-(cv-pv))

p3<- ggplot(data.frame(pr, cv, pv,rf), aes(x = pr)) +
  geom_line(aes(y = cv, color = "Call")) +
  geom_line(aes(y = pv, color = "Put")) +
  geom_line(aes(y = rf, color = "Strike-(Price-(c - p))")) +
  scale_color_manual(values=c("red", "blue","green")) +
  labs(title = "Price of options at T=1", 
       color = "", y = "Option Price", x = "Stock Price")

p3+ annotate("text", x = 100, y = 32,
             label = "S=100, K=100, r=0.05, T=1, sigma=0.33",
             color = "black", size = 3.5)+
  annotate("text", x = 101, y = 12.5,
           label = "Risk-free rate",
           color = "black", size = 4,hjust = 0)+
  geom_segment(aes(x = 100, y = 10.5, xend = 100, yend = 15.3),
               arrow = arrow(length = unit(0.12, "cm")))+
  annotate("text", x = 85, y = 5.5,
           label = "Risk-free rate",
           color = "black", size = 4,hjust = 0)

#Section V - put/call parity
# create a vector of underlying prices 
pr <- seq(from=100*0.75, to=100*1.25, by=0.1)
#apply those underlying prices to the Black_Scholes function, at default time-to-expiry (1 year)
cv <- Black_Scholes(S = pr, T=1)
pv <- Black_Scholes(S = pr,T=1,type="P")

#vector of calls less puts
cp <- (cv-pv)
stock <- pr-K

p4<- ggplot(data.frame(pr, cv, pv,cp, stock), aes(x = pr)) +
  geom_line(aes(y = cv, color = "Call")) +
  geom_line(aes(y = pv, color = "Put")) +
  geom_line(aes(y = stock, color = "Return: stock")) +
  geom_line(aes(y = cp, color = "Return: Call-Put")) +
  scale_color_manual(values=c("red", "blue","green","black")) +
  labs(title = "Put/call parity", 
       color = "", y = "Value", x = "Stock Price")

p4+ annotate("text", x = 100, y = 32,
             label = "S=100 (at start), K=100, r=0.05, T=1, sigma=0.33",
             color = "black", size = 3.5)
  