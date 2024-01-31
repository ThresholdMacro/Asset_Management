## ---------------------------
##
## Script name: CAPM
## Purpose of script: build betas from scratch, using PCE and 1m T.Bill rate to estimate rfr
## Author: Meyrick Chapman
## Date Created: 2022-12-18
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
library(alfred)
library(knitr)
library(kableExtra)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------
## load up our functions into memory
# Function to calculate monthly returns on a stock
monthly_stock_returns <- function(ticker, src, start_year) {
  
  # Download the data from Yahoo finance
  symbol <- getSymbols(ticker, src = src, 
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
start_date <- as.Date("2007-01-01")
year <- year(start_date)


monthly_stock_returns('DIS', 'yahoo',year)
monthly_stock_returns('GE', 'yahoo',year)
monthly_stock_returns('GM', 'yahoo',year)
monthly_stock_returns('IBM', 'yahoo',year)
monthly_stock_returns('MSFT', 'yahoo',year)
monthly_stock_returns('XOM', 'yahoo',year)
monthly_stock_returns('GOOG', 'yahoo',year)
monthly_stock_returns('JPM', 'yahoo',year)
monthly_stock_returns('AMZN', 'yahoo',year)
monthly_stock_returns('^GSPC', 'yahoo',year)
monthly_stock_returns('CPIAUCSL', 'FRED',year)

#get 1m  Treasury Bill yield
T1m <- getSymbols("DGS1MO", src = "FRED", auto.assign = FALSE, from = start_date)

T1m <- apply.monthly(T1m,last)

df <- data.frame(date=index(T1m), coredata(T1m)/100)
df$date <- lubridate::ceiling_date(df$date, "month")-1
rownames(df) <- c(seq(1:nrow(df)))

PCE <-data.frame(date=index(CPIAUCSL), coredata(CPIAUCSL))
PCE$date <- lubridate::ceiling_date(PCE$date, "month")-1

risk_free <- left_join(PCE,df, by='date')

risk_free <-
  risk_free %>%
  tidyr::fill(DGS1MO, .direction="down")

risk_free$rfr <- (risk_free$DGS1MO-risk_free$CPIAUCSL)

risk_free <- xts(risk_free,order.by = risk_free$date)
time(risk_free)<- time(risk_free) %>% as.yearmon() %>% as.Date()
capm <- merge.xts(DIS, GE, GM, IBM, MSFT, XOM, GOOG, JPM, AMZN, `^GSPC`)
time(capm)<- time(capm) %>% as.yearmon() %>% as.Date()
capm <- merge.xts(capm,risk_free$rfr)

summary(capm)

capm$risk.premium <- capm$X.GSPC - capm$rfr
knitr::kable(head(capm, 11), format = 'html') %>%
  kableExtra::kable_styling(bootstrap_options = c('striped', 'hover'))

ggplot(data = capm, aes(y = MSFT, x = risk.premium)) + geom_point(col='blue') + xlab('Risk Premium') + 
  ylab('Expected Return') + ggtitle('Microsoft Stock Returns') + geom_abline(method='lm')

DIS.fit <- lm(DIS ~ rfr + risk.premium, data = capm)
GE.fit <-  lm(GE ~ rfr + risk.premium, data = capm)
GM.fit <-  lm(GM ~ rfr + risk.premium, data = capm)
IBM.fit <-  lm(IBM ~ rfr + risk.premium, data = capm)
MSFT.fit <-  lm(MSFT ~ rfr + risk.premium, data = capm)
XOM.fit <-  lm(XOM ~ rfr + risk.premium, data = capm)
GOOG.fit <-  lm(GOOG ~ rfr + risk.premium, data = capm)
JPM.fit <-  lm(JPM ~ rfr + risk.premium, data = capm)
AMZN.fit <-  lm(AMZN ~ rfr + risk.premium, data = capm)
Mkt.fit <-  lm(X.GSPC ~ rfr + risk.premium, data = capm)

# beta coefficients
stock <- c('DIS', 'GE', 'GM', 'IBM', 'MSFT', 'XOM', 'GOOG', 'JPM', 'AMZN', 'Mkt')

beta <- c(DIS.fit$coefficients[3], 
                     GE.fit$coefficients[3],
                     GM.fit$coefficients[3],
                     IBM.fit$coefficients[3],
                     MSFT.fit$coefficients[3],
                     XOM.fit$coefficients[3],
                     GOOG.fit$coefficients[3],
                     JPM.fit$coefficients[3],
                     AMZN.fit$coefficients[3],
                     Mkt.fit$coefficients[3])

df <- data.frame(stock, beta)
kable(df, format = 'html') %>%
  kable_styling(bootstrap_options = c('striped', 'hover'))
