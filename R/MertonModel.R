## ---------------------------
##
## Script name: MertonModel
## Purpose of script: d2 as a measure of distance to default, and other applications
## Author: Meyrick Chapman
## Date Created: 2022-11-30
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
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
# Function to calculate Distance-to-default
d2 <- function(S=130, K=100, r=0.05, T= 1.00, sigma=0.20){
  d2 <- (log(S/K) + (r - sigma^2/2)*T) / (sigma*sqrt(T))
  
  return(d2)
}


# We now illustrate the function with the help of an example. 
# We input the mcap as 10000, debt as 5000,vol of equity a s0.4 and r as 0.10,
# for a hypothetical firm‘X’ as follows: 

V <- 130 #Firm value
D <- 100 #FV Debt
r <- 0.05 #Exp return
T <- 1.0 # Time in years (1 period)
sigma <- 0.20 #Volatility

#Current distance to default is ln(Firm value/Debt)
DD <- log(V/D)
# firm value would have to drop 26% to hit default 
# but debt is not maturing now, but in 1 year, 
# so what is the probaility of hitting the 100 strike in 1 year?
# answer is given by d2 in Black Scholes formula

#distance to default at end of period (1 year)
DD1period <- DD+(r-sigma^2/2)

# Distance to default after 1 year / sigma gives distance to default in standard deviations
DD1period/sigma
# which is the same as d2 (because we just decomposed the formula into constituent parts)
d2(S=130,K=100,r=0.05,T=1,sigma=0.20)
# d2 is the standard normal deviation from default level

# so what is the probability of hitting the strike? 
# create variable first
DD1 <- d2(S=130,K=100,r=0.05,T=1,sigma=0.20)
pnorm(-DD1)


pr <- seq(from=90, to=150, by=0.1)
pd <- pnorm(-d2(S=pr,K=100,r=0.05,T=1,sigma=0.2))
pd1 <- pnorm(-d2(S=pr,K=100,r=0.05,T=0.5,sigma=0.2))
pd2 <- pnorm(-d2(S=pr,K=100,r=0.05,T=0.25,sigma=0.2))
pd3 <- pnorm(-d2(S=pr,K=100,r=0.05,T=0.0575,sigma=0.2))

p<- ggplot(data.frame(pr, pd, pd1, pd3), aes(x = pr)) +
  geom_line(aes(y = pd, color = "1 year"))+
  geom_line(aes(y = pd1, color = "Six months"))+
  geom_line(aes(y = pd3, color = "Three weeks"))+
  scale_color_manual(values=c("red", "blue", "green")) +
  labs(title = "Merton model for corporate default rate", 
       color = "", y = "Probability of default", x = "Firm value")
p+ annotate("text", x = 120, y = 1,
             label = "S=variable, K=100, r=0.05, T=0, sigma=0.20",
             color = "black", size = 3.5)


# Section II, a fanciful (non-serious) estimation of probability of hitting lower bound of Excess Reserves at the Fed. 
# Below that limit nasty things are expected to happen
V <- 4130 # System reserves in Fed balance sheet at beginning of June 2022, when QT began
K <- 3000 # Estimated lower bound
r <- (-0.27) #expected decline in reserves over next year = 3*45+9*90 = 1140 = 36% of starting level
T <- 1.0 # Time in years (1 period)
sigma <- 0.12 #Volatility

#Current distance to default is ln(Firm value/Debt)
DD <- log(V/K)
# so what is the probability of hitting the strike? 
# create variable first
DD1 <- d2(S=V,K=K,r=(r),T=T,sigma=sigma)

pnorm(DD1)

D <- seq(from=1500, to=4130, by=10)
DD1 <- d2(S=V,K=K,r=(r),T=T,sigma=sigma)
pnorm(-DD1)

startQT <- 4189 # V # most recent level of Reserves, November 2022
plannedQT <- 2360
  # V-1140 # at $95 bln per month = 1140 bln per year
Res <- seq(from=plannedQT, to=startQT, length.out = 211)

# Lower bound of System Reserves = 2500, and other levels ($1390 was the level in September 2019 when repo had a funny turn)
pd <- pnorm(-d2(S=Res,K=K,r=(r),T=T,sigma=sigma))
pd1 <- pnorm(-d2(S=Res,K=2500,r=(-0.36),T=1,sigma=sigma))
pd2 <- pnorm(-d2(S=Res,K=2000,r=(-0.36),T=1,sigma=sigma))
pd3 <- pnorm(-d2(S=Res,K=1500,r=(-0.36),T=1,sigma=sigma))


p1<- ggplot(data.frame(Res, pd, pd1, pd2), aes(x = Res)) +
  geom_line(aes(y = pd, color = "Lower bound $3.0 trillion"))+
  geom_line(aes(y = pd1, color = "Lower bound $2.5 trillion"))+
  geom_line(aes(y = pd2, color = "Lower bound $2.0 trillion"))+
  scale_color_manual(values=c("yellow","red", "blue", "green")) +
  labs(title = "Reserves + ONRRP: lower bound of reserves in 1 year: Merton Model estimates", 
       caption = 'Source: Federal Reserve, HedgeAnalytics',
       color = "", y = "Probability of breaching lower bound", x = "Current system reserves, $bln")
p1 <-
  p1 + darktheme + theme(legend.position = 'bottom') + scale_color_HA_qualitative() + scale_y_continuous(limits = c(0, 1), labels = label_percent()) 
p1+ annotate("text", x = 2850, y = 0.05,
            label = "Current Reserves + ONRRP = 4130, K=variable, r=(27.5%), T=1, sigma=0.12",
            color = "white", size = 3.5)+
  annotate("text", x = max(Res), y = pd[length(pd)]+0.04,
          label = paste0(round(pd[length(pd)],2)*100,"%"),
          color = "white", size = 3.5)+
  annotate("text", x = max(Res), y = pd1[length(pd1)]+0.05,
           label = paste0(round(pd1[length(pd1)],2)*100,"%"),
           color = "white", size = 3.5)+
  annotate("text", x = max(Res), y = pd2[length(pd2)]+0.05,
           label = paste0(round(pd2[length(pd2)],2)*100,"%"),
           color = "white", size = 3.5)

# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Libs/HAgraphics/R/ggstdplots.R")


