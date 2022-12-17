## ---------------------------
##
## Script name: COFER
## Purpose of script: download and present Composition of Foreign Exchange Reserves from IMF
## Author: Meyrick Chapman
## Date Created: 2022-11-06
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
library(IMFData)
library(rdbnomics)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = Date,
           y = value,
           colour = key
         )) +
    geom_line() +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %Y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::comma)+
    theme(legend.position="right")+
    tidyquant::theme_tq()
}
ggstdpct<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value,
           colour = key
         )) +
    geom_line() +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %Y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::percent)+
    theme(legend.position="bottom")+
    tidyquant::theme_tq()
}

## ---------------------------

df4 <- rdb("IMF","COFER","Q.W00.")
 
df <-
df4 %>%
select(period,value,series=series_name) %>%
filter(year(period)>="1998-01-01") 
df <-pivot_wider(df, id_cols = 'period' ,names_from = "series", values_from = 'value')

names(df) <- gsub("Quarterly – All Countries, excluding the IO – ","",names(df))
names(df) <- gsub("Shares of Allocated Reserves, Shares "," Shares ",names(df))
names(df) <- gsub("Allocated Reserves, Claims "," Claims ",names(df))
colnames(df)[1] <- 'Date'
# df <- df[,order(colnames(df))]

Shares <- data.frame('Date'=df$Date, df[,grep("Shares ",names(df))])
Claims <- data.frame(df[,-grep("Shares ",names(df))])
names(Shares) <- gsub("X.","",names(Shares))
names(Shares) <- gsub("\\."," ",names(Shares))
names(Shares) <- gsub("  Percent","",names(Shares))
names(Claims) <- gsub("X.","",names(Claims))
names(Claims) <- gsub("\\."," ",names(Claims))
names(Claims) <- gsub("  US Dollars  Millions","",names(Claims))

lngClaims <-
  Claims %>%
  pivot_longer(c(-Date),names_to = 'key', values_to = 'value')

p <-
  ggstandard(
    lngClaims,
    "Composition of Foreign Exchange Reserves: Claims",
    "source: IMF",
    "5 years",
    "US$ mlns"
  )
p
library(plotly)
ggplotly(p)

lngShares <- 
  Shares[,-c(10,12)] %>%
  pivot_longer(c(-Date),names_to = 'key', values_to = 'value')

p1 <-
  ggstandard(lngShares,
             "Composition of Foreign Exchange Reserves: Shares",
             "source: IMF",
             "5 years",
             "%")
p1
ggplotly(p1)
