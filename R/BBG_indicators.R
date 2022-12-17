## ---------------------------
##
## Script name: BBG_indicators
## Purpose of script: download major data series according to Bloomberg, and test whether they have any impact on market prices, or each other
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
library(rdbnomics)
library(readxl)
library(corrplot)
library(vars)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
ggstandard<-function(Long_data,charttitle,chartcaption,xaxis_date_breaks,yaxis_title){
  ggplot(data = Long_data,
         aes(
           x = Index,
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

## ---------------------------

# a table of indicators and their sources 
indicators <- readr::read_csv("Data/MajEcon.csv")
# Global PMI is not readily available, 
# but the Conference Board provide a Global Economic Indicator which purports to do the same thing - signal evolution of global economy

for(rdb in which(indicators$Source == 'db.nomics.world')){
  df <- rdb(indicators$Provider[rdb],indicators$Dataset[rdb],indicators$Series[rdb])
  df1 <-pivot_wider(df, id_cols = 'period' ,names_from = "series_name", values_from = 'value')
  df1 <- assign(as.character(indicators$Short_name[rdb]), df1)
  
}

# create the series as shown by Bloomberg

US_Unemploy$MthlyChg <- 0
US_Unemploy$MthlyChg[2:nrow(US_Unemploy)] <-
  diff(
    US_Unemploy$`ALL EMPLOYEES, THOUSANDS – Total nonfarm – Seasonally Adjusted – Total nonfarm`,
    1
  )
US_Unemploy$BBG <- US_Unemploy$MthlyChg
US_Unemploy <- US_Unemploy %>% filter(period >= '2003-01-01')

US_Cons_Spend$mom <- 0
US_Cons_Spend$mom[2:nrow(US_Cons_Spend)] <-
  diff(log(US_Cons_Spend$`Market-based PCE (line 27) - Monthly`), 1)
US_Cons_Spend$BBG <- US_Cons_Spend$mom
US_Cons_Spend <- US_Cons_Spend %>% filter(period >= '2003-01-01')

US_PCE$yoy <- 0
US_PCE$yoy[13:nrow(US_PCE)] <-
  diff(log(US_PCE$`Market-based PCE (line 27) - Monthly`), 12)
US_PCE$BBG <- US_PCE$yoy
US_PCE <- US_PCE %>% filter(period >= '2003-01-01')

Brazil_GDP$BBG <-
  Brazil_GDP$`Brazil – Leading Indicators OECD > Reference series > Gross Domestic Product (GDP) > Original series – Growth rate same period previous year, s.a. – Quarterly` /
  100
Brazil_GDP <- Brazil_GDP %>% filter(period >= '2003-01-01')

ChileCopperExp <- read_excel("Data/ChileCopperExportsTons.xlsx", 
                                     skip = 1)

colnames(ChileCopperExp)[1] <- 'date'
CopperperTon <- alfred::get_fred_series(series_id = 'PCOPPUSDM',series_name = 'Copper $ per ton' )

ChileCopperExp <- left_join(ChileCopperExp,CopperperTon,by='date')
ChileCopperExp$BBG <- ChileCopperExp$`Exportaciones mes`*ChileCopperExp$`Copper $ per ton`
colnames(ChileCopperExp)[1] <- 'period'
ChileCopperExp <- ChileCopperExp %>% filter(period >= '2003-01-01')

EZ_HICP$BBG <- 0
EZ_HICP$BBG[13:nrow(EZ_HICP)] <-
  diff(
    log(
      EZ_HICP$`Monthly – Index, 2005=100 – All-items HICP – Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)`
    ),
    12
  )
EZ_HICP <- EZ_HICP %>% filter(period >= '2003-01-01')

IFO <- read_excel("Data/IFO11_2022.xlsx", 
                         sheet = "ifo Business Climate", skip = 7)

IFO <- IFO[2:nrow(IFO),]
IFO$BBG <- IFO$`Business Climate...2`  
IFO$period <- as.Date(my(IFO$`Month/year`))
IFO <- IFO %>% filter(period >= '2003-01-01')


China_CaixinPMI <- readr::read_csv("Data/China_Manu_PMI.csv")
China_CaixinPMI$period <- as.Date(my(China_CaixinPMI$period))
China_CaixinPMI$BBG <- China_CaixinPMI$`China PMI`

#China data needs joining
China_PPI.complete <-
  data.frame('period' = seq(
    from = min(China_PPI_OECD$period),
    to = max(China_PPI$period),
    by = 'months'
  ))
China_PPI.complete <-
  left_join(China_PPI.complete, China_PPI_OECD, by = 'period')
China_PPI.complete <-
  left_join(China_PPI.complete, China_PPI, by = 'period')
colnames(China_PPI.complete) <- c("period", 'PPI_OECD', 'PPI')
China_PPI.complete$PPI <- China_PPI.complete$PPI - 100

China_PPI.complete <-
  China_PPI.complete %>%
  mutate(BBG = coalesce(PPI_OECD, PPI))

China_PPI.complete <- China_PPI.complete %>% filter(period >= '2003-01-01')

Japan_CPI$BBG <- 0
Japan_CPI$BBG[13:nrow(Japan_CPI)] <-
  diff(log(Japan_CPI$`All items`), 12)
Japan_CPI <- Japan_CPI %>% filter(period >= '2003-01-01')

Korea_Exportsyoy$BBG <-
  Korea_Exportsyoy$`Korea – International Trade > Exports > Value (goods) > Total – Growth rate same period previous year, s.a. – Monthly` /
  100
Korea_Exportsyoy <- Korea_Exportsyoy %>% filter(period >= '2003-01-01')

colsselected <-c('period','BBG')

comb<- left_join(US_Unemploy[,colsselected],US_PCE[,colsselected], by = 'period')
colnames(comb)[2] <- 'US Employment'
colnames(comb)[3] <- 'US PCE Prices'
comb <- left_join(comb,US_Cons_Spend[,colsselected], by = 'period')
colnames(comb)[4] <- 'US Consumer Spend'
comb <- left_join(comb,ChileCopperExp[,colsselected], by = 'period')
colnames(comb)[5] <- 'Chile Copper Exp in USD'
comb <- left_join(comb,EZ_HICP[,colsselected], by = 'period')
colnames(comb)[6] <- 'Eurozone HICP'
comb <- left_join(comb,IFO[,colsselected], by = 'period')
colnames(comb)[7] <- 'IFO'
comb <- left_join(comb,China_CaixinPMI[,colsselected], by = 'period')
colnames(comb)[8] <- 'China Caixin PMI'
comb <- left_join(comb,China_PPI.complete[,colsselected], by = 'period')
colnames(comb)[9] <- 'China PPI'
comb <- left_join(comb,Japan_CPI[,colsselected], by = 'period')
colnames(comb)[10] <- 'Japan CPI'
comb <- left_join(comb,Korea_Exportsyoy[,colsselected], by = 'period')
colnames(comb)[11] <- 'Korea Exports'

Mktdata <- readr::read_csv("Data/MktMthly.csv")

Mktchg <- data.frame(apply(Mktdata[,2:ncol(Mktdata)],2,function(x) diff(log(x), lag=1)))
Mktchg$period <- Mktdata$period[2:nrow(Mktdata)]

comb <-
  comb %>%
  filter(period >= min(Mktchg$period)) %>%
  filter(period <= max(Mktchg$period))

par(mfrow=c(4, 5), mar=c(3, 1, 3, 1))  ## create a 4x4 plot

ccf(Mktchg$SPX.Index,comb$`US Employment`,type = 'correlation', main='NFP',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`US PCE Prices`,type = 'correlation', main='PCE Prices',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`US Consumer Spend`,type = 'correlation', main='US Consumer Spend',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`Chile Copper Exp in USD`,type = 'correlation', main='Chile copper exports',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`Eurozone HICP`,type = 'correlation', main='Eurozone HICP',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$IFO,type = 'correlation', main='IFO',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`China Caixin PMI`,type = 'correlation', main='China Caixin PMI',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`China PPI`,type = 'correlation', main='China PPI',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`Japan CPI`,type = 'correlation', main='Japan CPI',na.action = na.pass)
ccf(Mktchg$SPX.Index,comb$`Korea Exports`,type = 'correlation', main='Korean Exports',na.action = na.pass)

ccf(Mktchg$TY1.Comdty,comb$`US Employment`,type = 'correlation', main='NFP',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`US PCE Prices`,type = 'correlation', main='PCE Prices',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`US Consumer Spend`,type = 'correlation', main='US Consumer Spend',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`Chile Copper Exp in USD`,type = 'correlation', main='Chile copper exports',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`Eurozone HICP`,type = 'correlation', main='Eurozone HICP',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$IFO,type = 'correlation', main='IFO',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`China Caixin PMI`,type = 'correlation', main='China Caixin PMI',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`China PPI`,type = 'correlation', main='China PPI',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`Japan CPI`,type = 'correlation', main='Japan CPI',na.action = na.pass, col='red')
ccf(Mktchg$TY1.Comdty,comb$`Korea Exports`,type = 'correlation', main='Korean Exports',na.action = na.pass, col='red')


par(mfrow=c(1, 1))
    
df <- left_join(Mktchg,comb, by='period')

tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))

# Remove dates using subset
df2 <- subset(df, select = -c(period))

df2 |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
           tl.cex=0.6, cl.cex = 0.6)

# Some NAs mess up the calculation so lets shorten the series
df <- 
  df %>%
  filter(period >= '2012-03-01')

df2 <- subset(df, select = -c(period))

df2 |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
           tl.cex=0.6, cl.cex = 0.6)

# better but still values missing from China PPI

# lessons? 


# #impulse response of selected BBG data
# shtcomb <-
#   comb %>%
#   filter(period >="2012-03-01")
# 
# #add S&P500 data
# mkts <- Mktchg[,c(1,11)]
# shtcomb <- left_join(shtcomb,mkts, by='period')
# 
# #fill in a couple of NA 
# shtcomb <- tidyr::fill(shtcomb,names(shtcomb))
# 
# # Estimate model for vector autoregression tests
# # remove date column 
# 
# model <- vars::VAR(shtcomb[,-1], p = 2, type = "const")
# 
# # Look at summary statistics
# feir <- irf(model, impulse = "US.Employment", response = "SPX.Index",
#             n.ahead = 8, ortho = FALSE, runs = 1000)
# 
# plot(feir)
# 
# feir <- irf(model, impulse = "US.PCE.Prices", response = "SPX.Index",
#             n.ahead = 8, ortho = FALSE, runs = 1000)
# 
# plot(feir)

