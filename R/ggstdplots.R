## ---------------------------
##
## Script name: ggstdplots
## Purpose of script: collection of ggplot2 scripts for generic chart generation
## Author: Meyrick Chapman
## Date Created: 2021-10-21
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

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(patchwork)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
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

ggstdcol<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = as.Date(Date),
           y = value)) +
    geom_col(fill = 'steelblue') +
    labs(title = charttitle,
         caption = chartcaption) +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %Y",
                 name = "Date") +
    scale_y_continuous(name = yaxis_title, labels = scales::percent)+
    theme(legend.position="bottom")+
    tidyquant::theme_tq()
}

gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", xaxis_date_breaks = "6 months",plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj  
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
    y
  }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Index","Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Index")) +
      geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "coral3") +
      ggtitle(title.string) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.background = element_rect(fill = "steelblue2", colour = "steelblue2"),
        strip.background = element_rect(fill = "wheat2"),
        plot.background = element_rect(fill = "steelblue2"),
        panel.grid.major=element_line(colour="steelblue2"),
        panel.grid.minor=element_line(colour="steelblue2")
      ) +
      scale_x_datetime(breaks = date_breaks(xaxis_date_breaks), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
    
  } 
  else 
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                   , labeller = label_value) + # label_value is default
      
      # display points for Index and Drawdown, but not for Return
      geom_point(data = subset(df, variable == c("Index","Drawdown"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) + 
      
      # manually select shape of geom_point
      scale_shape_manual(values = c(1,2,3)) + 
      
      # line colours for the Index
      geom_line(data = subset(df, variable == "Index"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # bar colours for the Return
      geom_bar(data = subset(df,variable == "Return"), stat = "identity"
               , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
      
      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks(xaxis_date_breaks), labels = date_format("%m/%Y")) +
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend 
    
    gglegend <- guide_legend(override.aes = list(size = 3))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect(colour = 'grey')
             , legend.key = element_rect(fill = "white", colour = "white")
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = "white")
             , panel.background = element_rect(fill = "white", colour = "white")
             , panel.grid.major = element_line(colour = "grey", size = 0.5) 
             , panel.grid.minor = element_line(colour = NA, size = 0.0)
      )
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}



#Customized labels can be obtained with the following function:
  
  # create a function to store fancy axis labels 
  
my_labeller <- function(var, value){ # from the R Cookbook
    value <- as.character(value)
    if (var=="variable") 
    {
      value[value=="Index"] <- "Cumulative Returns"
      value[value=="Return"] <- "Daily Returns"
      value[value=="Drawdown"] <- "Drawdown"
    }
    return(value)
  }




gg.charts.PerformanceSummary2<- function(rtn.obj, main = "Asset performance", xaxis_date_breaks = "6 months",plot = TRUE){

  colnames(rtn.obj)[1] <- 'value'
  cum_returns <- cumprod(rtn.obj + 1) - 1
  colnames(cum_returns)[1] <- 'value'
  drawdown <-
    PerformanceAnalytics:::Drawdowns(rtn.obj)
  colnames(drawdown)[1] <- 'value'
  
  p1 <-
    ggplot(cum_returns, aes(x = Index, y = value)) + geom_line() +
    labs(title = "Cumulative return") +
    geom_hline(yintercept = 0, size = 0.5, colour = "coral3") +
    theme(
      panel.border = element_rect(fill = NA, colour = "steelblue2"),
      panel.background = element_rect(fill = "steelblue2", colour = "steelblue2"),
      strip.background = element_rect(fill = "wheat2"),
      plot.background = element_rect(fill = "steelblue2"),
      panel.grid.major = element_line(colour = "steelblue3"),
      panel.grid.minor = element_line(colour = "steelblue2"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    scale_y_continuous(position = "right") +
    ylab("") +
    xlab("")
  
  p2 <-
    ggplot(rtn.obj, aes(x = Index, y = value)) + geom_bar(stat = "identity") +
    labs(title = "Monthly return") +
    geom_hline(yintercept = 0, size = 0.5, colour = "coral3") +
    theme(
      panel.background = element_rect(fill = "steelblue2", colour = "steelblue2"),
      strip.background = element_rect(fill = "wheat2"),
      plot.background = element_rect(fill = "steelblue2"),
      panel.grid.major = element_line(colour = "steelblue3"),
      panel.grid.minor = element_line(colour = "steelblue2"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    scale_y_continuous(position = "right") +
    ylab("") +
    xlab("")
  
  p3 <- ggplot(drawdown, aes(x = Index, y = value)) + geom_line() +
    labs(title = "Drawdown") +
    geom_hline(yintercept = 0, size = 0.5, colour = "coral3") +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.background = element_rect(fill = "steelblue2", colour = "steelblue2"),
      strip.background = element_rect(fill = "wheat2"),
      plot.background = element_rect(fill = "steelblue2"),
      panel.grid.major = element_line(colour = "steelblue3"),
      panel.grid.minor = element_line(colour = "steelblue2")
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = xaxis_date_breaks,
                 date_labels =  "%b %Y",
                 name = "Date") +
    ylab("") +
    xlab("")
  
  
  comboplot <- p1 / p2 / p3 +
    plot_annotation(title = 'Portfolio performance',
                    theme = theme(
                      plot.background = element_rect(color  = 'steelblue2', size = 2, fill =
                                                       "steelblue2")
                    )) +
    plot_layout(heights = c(2, 0.5, 0.5))
  
  if (plot == TRUE) {
    plot(comboplot)
  } else {
  }
  
}
