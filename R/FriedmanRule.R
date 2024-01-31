## ---------------------------
##
## Script name: FriedmanRule
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: Sun Jun 04 2023
## Copyright (c) Hedge Analytics Ltd, 2023
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
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyr
               , plyr
               , dplyr
               , lubridate
               , ggplot2
               , ggthemes
               , readr
               , rdbnomics
               , alfred
               ,timetk)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
get.mav <- function(bp,n=2){
  require(zoo)
  if(is.na(bp[1])) bp[1] <- mean(bp,na.rm=TRUE)
  bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(bp)
  c(bp[1:(n-1)],rollapply(bp,width=n,mean,align="right"))  
}

## ---------------------------

M2 <- get_fred_series("WM2NS", series_name = 'M2', observation_start = '1998-01-01')
ImplicitDefl <-get_fred_series("GDPDEF", series_name = 'GDP Deflator', observation_start = '1998-01-01')
RealGDP <-get_fred_series("GDPC1", series_name = 'Real GDP', observation_start = '1998-01-01')
M2Velocity <-get_fred_series("M2V", series_name = 'M2 Velocity', observation_start = '1998-01-01')

QtrlyM2 <-
  M2 %>%
  group_by(`date`) %>%
  summarise_by_time(
    .date_var = `date`,
    .by       = "quarter",
    # Setup for monthly aggregation
    # Summarization
    QtrlyM2  = last(M2))

data <- left_join(ImplicitDefl,RealGDP, by= 'date')
data <- left_join(data,M2Velocity, by='date')
data <- left_join(data,QtrlyM2, by='date')

data$logDef[5:nrow(data)] <- diff(log(data$`GDP Deflator`),lag = 4)
data$logGDP[5:nrow(data)] <- diff(log(data$`Real GDP`),lag=4)
data$logGDP4yMA <- get.mav(data$logGDP,n=16)
data$logM2Vel[5:nrow(data)] <- diff(log(data$`M2 Velocity`), lag=4)
data$logM2[5:nrow(data)] <- diff(log(data$QtrlyM2), lag=4)

data$FriedmanRule <- (data$logDef+data$logGDP)-data$logM2Vel
data$FriedmanFixed2pctInfl <- (0.02+data$logGDP4yMA)-(-0.01)

plot(data$date, data$logM2, type = 'l', main = 'M2 4 quarter log change', xlab= 'Date', ylab = '% change')
lines(data$date, data$FriedmanRule, col = 'red')
lines(data$date, data$FriedmanFixed2pctInfl, col = 'blue')
lines(data$date,data$logGDP4yMA, col='green')
abline(0.02, 0, col='brown')
abline(-0.01, 0, col='pink')
legend("topleft", legend=c('M2 yoy', 'FriedmanRule',"Friedman Fixed 2% inflation","GDP 4y MA","Inflation target yoy","Velocity change yoy"),col = c("black","red", "blue", "green", "brown","pink"),lty=1)
legend("topleft", c("M2", "Friedman Rule on data", "Static Friedman Rule (2+GDP 4y MA)+1)"), lty=1,col = c('black', 'red','blue'))


# use changing trend growth rate - Hodrick-Prescott filter or 3-5 year MA of GDP