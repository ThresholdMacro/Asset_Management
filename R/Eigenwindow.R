## ---------------------------
##
## Script name: Eigenwindow
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: 2021-09-09
## Copyright (c) Hedge Analytics Ltd, 2021
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 6) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(xts)
library(missMDA)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(gifski)
library(gganimate)
library(ggthemes)
library(rdbnomics)
library(ggthemes)
library(tidyquant)
library(quantmod)
library(PerformanceAnalytics)
library(gifski)
library(tseries)
# source("functions/packages.R")       # loads up all the packages we need
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

df<-read.csv("Data/mthlydata.csv")
#df<-df[,c(1,2:ncol(df))]

#scaleddf<-data.frame(lapply(df[,2:ncol(df)],m_rescale,RESCALELB,RESCALEUB))

data_used<-xts(df[,2:ncol(df)], order.by = as.Date(ymd(df$period)))

index(data_used)

Eigvaluedf <- data.frame("Dim1"=0,"Dim2"=0,"Dim3"=0,"Dim4"=0,"Dim5"=0)  
Eigvectordf <- data.frame("Dim1"=0,"Dim2"=0,"Dim3"=0,"Dim4"=0,"Dim5"=0)  
startrow <- 1
endrow  <- 12
for(block in 1:(nrow(data_used)-endrow)){
  
  datablock <- data_used[startrow:endrow,1:ncol(data_used)]
  
  pc1<-PCA(datablock,graph = FALSE)
  Eigvalues <- pc1$eig[1:5]  
  Eigcoord <- get_pca_var(pc1)
  Eigvectors <- Eigcoord$coord[10,]
  Eigvectordf <- rbind(Eigvectordf, Eigvectors)
  Eigvaluedf <- rbind(Eigvaluedf,Eigvalues)
  
  startrow <- startrow + 1
  endrow <- endrow + 1 
}

readr::write_csv(Eigvaluedf, "Data/EigvalueHistory1ywindow.csv")
readr::write_csv(Eigvectordf, "Data/EigvectorHistory1ywindow.csv")

eig1y <- readr::read_csv("Data/EigvalueHistory1ywindow.csv")
eig1y$date <- as.Date(df$period[12:nrow(df)])
eig1y[,1:5] <- eig1y[,1:5]/ncol(data_used) #divide by 38 (number of variables) to get % contribution to system of each eigenvalue 
eig1y <- eig1y[2:nrow(eig1y),]

lngeig1y <-
  eig1y %>%
  pivot_longer(c(-date), names_to = 'key', values_to = 'value')

p <- ggstdpct(lngeig1y,"PCA contribution to levels of financial universe, 2003-2022: 12 mth window (with trends + means)", "source: Hedge Analytics", "3 years", "% contribution")
p <- p + geom_smooth()
p + geom_hline(yintercept=colMeans(eig1y[,1:5]))



adf.test(eig1y$Dim1)
adf.test(eig1y$Dim2)
adf.test(eig1y$Dim3)
adf.test(eig1y$Dim4)
adf.test(eig1y$Dim5)

