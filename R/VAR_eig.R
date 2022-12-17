## ---------------------------
##
## Script name: VAR_eig
## Purpose of script: vector autoregression and associated tests on dimensions.
## Author: Meyrick Chapman
## Date Created: 2022-11-14
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##    this work taken from the following website:
##   https://towardsdatascience.com/a-deep-dive-on-vector-autoregression-in-r-58767ebb3f06
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
library(readr)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
#library(tidyverse)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Scripts/ggstdplots.R")
## ---------------------------

mp <- read_csv("Data/EigvalueHistory1ywindow.csv")
mp <- mp[2:nrow(mp),]

df<-read.csv("Data/mthlydata.csv")
startdate <- as.Date(df$period[14])
enddate <- as.Date(df$period[nrow(df)])

mpdates <- seq(startdate,enddate, by='1 month')

eig1 <- ts(mp$Dim1, start = c(2004,1,1), frequency = 12)
eig2 <- ts(mp$Dim2, start = c(2004,1,1), frequency = 12)
eig3 <- ts(mp$Dim3, start = c(2004,1,1), frequency = 12)
eig4 <- ts(mp$Dim4, start = c(2004,1,1), frequency = 12)
eig5 <- ts(mp$Dim5, start = c(2004,1,1), frequency = 12)

pp.test(eig1)
pp.test(eig2)
pp.test(eig3)
pp.test(eig4)
pp.test(eig5)

v1 <- cbind(eig1,eig2,eig3,eig4,eig5)
colnames(v1) <- cbind( "eig1","eig2","eig3", "eig4","eig5")
lagselect <- VARselect(v1, lag.max = 15, type = "const")

mp$mth <-
  seq(
    from = 1,
    to = nrow(mp)
  )


lngdata_used <-
  log(mp[,c(1:3,6)]) %>%
  pivot_longer(c(-mth),names_to = "key",values_to = 'value')

# Add mean, last, std dev
mu <- ddply(lngdata_used, "key", summarise, grp.mean=mean(value))
last. <-ddply(lngdata_used, "key", summarise, grp.last=last(value))
stdev. <- ddply(lngdata_used, "key", summarise, grp.sd=sd(value))
stdev.up <- data.frame('key'= stdev.$key, 'grp.sdup' = mu$grp.mean + stdev.$grp.sd)
stdev.dn <- data.frame('key'= stdev.$key, 'grp.sddn' = mu$grp.mean - stdev.$grp.sd)


p<-ggplot(lngdata_used, aes(x=value, fill=key, color=key)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 0.025)+
  facet_grid(key ~ .)
# Add mean lines
p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=key),
                linetype="dashed")
p<-p+geom_vline(data=stdev.up, aes(xintercept=grp.sdup, color=key),
                linetype="dotted")
p<-p+geom_vline(data=stdev.dn, aes(xintercept=grp.sddn, color=key),
                linetype="dotted")
p + geom_vline(data = last.,
               aes(xintercept = grp.last, color = key),
               linetype = "solid") +
  labs(title = "Dimension histogram plot (mean = dashed vertical line, latest = solid vertical line)", x =
         "Level, log", y = "Count") +
  theme_classic()


Model1 <- vars::VAR(v1, p = 2, type = "const", season = NULL, exog = NULL) 
summary(Model1)

Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1

Norm1 <- normality.test(Model1, multivariate.only = TRUE)

Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

plot(mpdates, Stability1$stability$eig1$process, type='l', main='OLS-CUSUM: Dim1')
plot(mpdates, Stability1$stability$eig2$process, type='l', main='OLS-CUSUM: Dim2')
plot(mpdates, Stability1$stability$eig3$process, type='l', main='OLS-CUSUM: Dim3')
plot(mpdates, Stability1$stability$eig4$process, type='l', main='OLS-CUSUM: Dim4')
plot(mpdates, Stability1$stability$eig5$process, type='l', main='OLS-CUSUM: Dim5')

Grangereig1 <- causality(Model1, cause = "eig1")
Grangereig1
Grangereig2 <- causality(Model1, cause = "eig2")
Grangereig2
Grangereig3 <- causality(Model1, cause = "eig3")
Grangereig3
Grangereig4 <- causality(Model1, cause = "eig4")
Grangereig4
Grangereig5 <- causality(Model1, cause = "eig5")
Grangereig5

forecast <- predict(Model1, n.ahead = 7, ci = 0.95)
par(mfrow = c(3, 2))

fanchart(forecast, names = "eig1", main = "Fanchart for Dim 1", xlab = "Horizon", ylab = "eig1")
fanchart(forecast, names = "eig2", main = "Fanchart for Dim 2", xlab = "Horizon", ylab = "eig2")
fanchart(forecast, names = "eig3", main = "Fanchart for Dim 3", xlab = "Horizon", ylab = "eig3")
fanchart(forecast, names = "eig4", main = "Fanchart for Dim 4", xlab = "Horizon", ylab = "eig4")
fanchart(forecast, names = "eig5", main = "Fanchart for Dim 5", xlab = "Horizon", ylab = "eig5")

eig1irf <- irf(Model1, impulse = "eig1", response = "eig1", n.ahead = 20, boot = TRUE)
eig2irf <- irf(Model1, impulse = "eig1", response = "eig2", n.ahead = 20, boot = TRUE)
eig3irf <- irf(Model1, impulse = "eig1", response = "eig3", n.ahead = 20, boot = TRUE)
eig4irf <- irf(Model1, impulse = "eig1", response = "eig4", n.ahead = 20, boot = TRUE)
eig5irf <- irf(Model1, impulse = "eig1", response = "eig5", n.ahead = 20, boot = TRUE)

plot(eig1irf, ylab = "Dim 1", main = "Dim 1's shock to Dim 1")
plot(eig2irf, ylab = "Dim 2", main = "Dim 1's shock to Dim 2")
plot(eig3irf, ylab = "Dim 3", main = "Dim 1's shock to Dim 3")
plot(eig4irf, ylab = "Dim 4", main = "Dim 1's shock to Dim 4")
plot(eig5irf, ylab = "Dim 5", main = "Dim 1's shock to Dim 5")


