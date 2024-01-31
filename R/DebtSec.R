## ---------------------------
##
## Script name: DebtSec.R
## Purpose of script: Non-resdident debt securities issued in dollars foreign currency, source: BIS, IMF
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
library(rdbnomics)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(tis)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
expand_data <- function(x) {
  years <- min(x$year):max(x$year)
  quarters <- 1:4
  grid <- expand.grid(quarter=quarters, year=years)
  x$quarter <- 1
  merged <- grid %>% left_join(x, by=c('year', 'quarter'))
  merged$key <- x$key[1]
  return(merged)
}
interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$y
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$int <- interpolation$y
  return(data)
}

expand_and_interpolate <- function(x) interpolate_data(expand_data(x))

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
DOWNLOAD <- TRUE
#  default to ALl issuers, unless specified here
NONFIN <- FALSE
GOVTSEC <- FALSE

Code_map<-readr::read_csv("Data/Code_map.csv")
fordebtsec <- readr::read_csv("Data/All_Issuers_foreign_debtsec.csv")
CountriesByContinent <- readr::read_csv("Data/allcountries.csv")
type <- 'all issuers'

if(GOVTSEC == TRUE){
  fordebtsec <- read_csv("Data/GenGovt_foreign_debtsec.csv")
  CountriesByContinent <- read_csv("Data/gengovtcountries.csv")
  type <- 'general government'
}

if(NONFIN == TRUE){
  fordebtsec <- read_csv("Data/NonFinCorp_foreign_debtsec.csv")
  CountriesByContinent <- read_csv("Data/nonfincountries.csv")
  type <- 'non financial'
}

if(DOWNLOAD == TRUE) {
  for (row in 1:nrow(Code_map)) {
    filter1 <- as.character(Code_map$Series_Code[row])
    df <-
      rdb(
        as.character(Code_map$Provider_ID[row]),
        as.character(Code_map$Series_Dataset[row]),
        id = filter1
      )
    # names(df) <- tolower(names(df))
    df <-
      pivot_wider(
        df,
        id_cols = 'period' ,
        names_from = `Issuer residence`,
        values_from = 'value'
      )
    
    df1 <- assign(as.character(Code_map$Short_name[row]), df)
    write.csv(df1,
              file = paste0('Data/', as.character(Code_map$Short_name[row]), '.csv'),
              row.names = FALSE)
    
  }
}


# Get IMF World Economic Outlook estimates of World NGDP in dollars 
WorldNGDP <- rdb('IMF','WEOAGG:2023-10','001.NGDPD.us_dollars')
WorldNGDP <- pivot_wider(
  WorldNGDP,
  id_cols = 'period' ,
  names_from = `WEO Countries group`,
  values_from = 'value'
)

# Get IMF World Economic Outlook estimates of Advanced Economies NGDP in dollars 
AdEconNGDP <- rdb('IMF','WEOAGG:2023-10','110.NGDPD.us_dollars')
AdEconNGDP <- pivot_wider(
  AdEconNGDP,
  id_cols = 'period' ,
  names_from = `WEO Countries group`,
  values_from = 'value'
)

# Get IMF World Economic Outlook estimates of Emerging and developing economies NGDP in dollars 
EmEconNGDP <- rdb('IMF','WEOAGG:2023-10','200.NGDPD.us_dollars')
EmEconNGDP <- pivot_wider(
  EmEconNGDP,
  id_cols = 'period' ,
  names_from = `WEO Countries group`,
  values_from = 'value'
)


df <- merge(WorldNGDP,AdEconNGDP, by='period')
df <- merge(df,EmEconNGDP, by='period')

NGDP_All <- df

colnames(NGDP_All)[1] <- 'Date'

nrow(NGDP_All)
1980+nrow(NGDP_All)-1
NGDP_All$Date <- c(1980:(1980+nrow(NGDP_All)-1))
rownames(NGDP_All)[1:(nrow(NGDP_All))] <- seq(1:nrow(NGDP_All))

lngNGDP_All <-
NGDP_All %>%
  pivot_longer(c(-Date), names_to = 'key', values_to = 'y')

colnames(lngNGDP_All)[1] <- 'year'

quarterly_data <- lngNGDP_All %>% group_by(key) %>% do(expand_and_interpolate(.))
quarterly_data$month <- quarterly_data$quarter*3

quarterly_data$date <-
  as.Date(lubridate::ymd(paste0(
    quarterly_data$year, "-", quarterly_data$month, "-01"
  )))
NGDPqtly <-
  quarterly_data %>% filter(date < '2023-01-01')
NGDPqtly$date <- ceiling_date(NGDPqtly$date, "quarter") - 1
NGDPqtly <-
  NGDPqtly %>%
  pivot_wider(id_cols='date', names_from = 'key', values_from = 'int')

All_Issuers_foreign_debtsec$date <-
  ceiling_date(All_Issuers_foreign_debtsec$period, "quarter") - 1

All_Issuers_foreign_debtsec[, c(2:160)] <-
  All_Issuers_foreign_debtsec[, c(2:160)] / 1000

selectedregions <-
  All_Issuers_foreign_debtsec |>
  dplyr::select(
    period,
    `All countries excluding residents`,
    `Developed countries`
  )

colnames(selectedregions) <-
  c("date",
    "Total eurobonds outstanding",
    "Developed countries eurobond outstanding")

selectedregions$date <- as.Date(lubridate::ceiling_date(selectedregions$date, unit = 'quarters')-days(1))

eurobondsGDP <- left_join(NGDPqtly, selectedregions, by = 'date')
eurobondsGDP$`Emerging countries eurobond outstanding` <-
  eurobondsGDP$`Total eurobonds outstanding` - eurobondsGDP$`Developed countries eurobond outstanding`
eurobondsGDP$`Eurobonds as pct World GDP` <-
  eurobondsGDP$`Total eurobonds outstanding` / eurobondsGDP$World
eurobondsGDP$`Eurobonds as pct AE GDP` <-
  eurobondsGDP$`Developed countries eurobond outstanding` / eurobondsGDP$`Advanced economies`
eurobondsGDP$`Eurobonds as pct EM GDP` <-
  eurobondsGDP$`Emerging countries eurobond outstanding` / eurobondsGDP$`Emerging market and developing economies`

lngeurobondsGDP <-
  dplyr::select(
    eurobondsGDP,
    date,
    `Eurobonds as pct World GDP`,
    `Eurobonds as pct AE GDP`,
    `Eurobonds as pct EM GDP`
  ) %>%
  pivot_longer(c(-date), names_to = 'key', values_to = 'value')

p <- ggstdpct(lngeurobondsGDP,"Eurobonds outstanding as % of GDP", "source: BIS, IMF", "5 years", "Percentage of nominal GDP")
p


# get dates of recessions from Fred site, https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/recession-bars/

# but I've downloaded that data already and stored in Data/recessions.csv

recessions <- read_csv("Data/recessions.csv")

#trim recession date to right length
recessions.trim = subset(recessions, Peak >= min(lngeurobondsGDP$date) )

p <- p +
  geom_rect(data = recessions.trim,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.5,
            inherit.aes = FALSE)

# source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Scripts/ggstdplots.R")

p
