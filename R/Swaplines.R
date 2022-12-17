## ---------------------------
##
## Script name: Swaplines
## Purpose of script: retrieve and plot Federal Reserve swap lines with other central banks using Fred data
## Author: Meyrick Chapman
## Date Created: 2022-12-07
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
library(alfred)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(date),
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
           x = as.Date(date),
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

swaplines <- get_fred_series('SWPT', series_name = 'CB swap lines')

# get dates of recessions from Fred site, https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/

# but I've downloaded that data already and stored in Data/recessions.csv

recessions <- read_csv("Data/recessions.csv")

#trim recession date to right length
recessions.trim = subset(recessions, Peak >= min(swaplines$date) )

lngswaplines <-
  swaplines %>%
  pivot_longer(c(-date), names_to = 'key', values_to = 'value')

#plot swap line usage
p <- ggstandard(lngswaplines,"Federal Reserve swap lines with other central banks", "source: Federal Reserve", "5 years", "US$ mln")

#overlay recessions
p <- p +
  geom_rect(data = recessions.trim,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5,
            inherit.aes = FALSE)
p
  