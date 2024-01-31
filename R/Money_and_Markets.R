## ---------------------------
##
## Script name: Money_and_Markets
## Purpose of script: show impact of various measures of money on global asset prices
## Author: Meyrick Chapman
## Date Created: 2022-12-09
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
library(corrplot)
library(vars)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

Alldata <- readr::read_csv("Data/UniverseData.csv")

Mktchg1 <- data.frame(apply(Alldata[,2:ncol(Alldata)],2,function(x) diff(log(x), lag=6)))
Mktchg1$period <- Alldata$period[7:nrow(Alldata)]

par(mfrow=c(4, 4), mar=c(3, 1, 3, 1))  ## create a 4x4 plot

FedEq <- ccf(Mktchg1$SPX.Index,Mktchg1$Fed,type = 'correlation', main='Fed reserves',na.action = na.pass)
ECBEq <- ccf(Mktchg1$SPX.Index,Mktchg1$ECB,type = 'correlation', main='ECB reserves',na.action = na.pass)
BoJEq <- ccf(Mktchg1$SPX.Index,Mktchg1$BoJ,type = 'correlation', main='BoJ reserves',na.action = na.pass)
PBoCEq <- ccf(Mktchg1$SPX.Index,Mktchg1$PBoC.Reserves,type = 'correlation', main='PBoC reserves',na.action = na.pass)
USM3Eq <- ccf(Mktchg1$SPX.Index,Mktchg1$US_M3,type = 'correlation', main='US M3',na.action = na.pass)
EZM3Eq <- ccf(Mktchg1$SPX.Index,Mktchg1$EZ_M3,type = 'correlation', main='EZ M3',na.action = na.pass)
JPM3Eq <- ccf(Mktchg1$SPX.Index,Mktchg1$JP_M3,type = 'correlation', main='Japanese M3',na.action = na.pass)
CNM3Eq <- ccf(Mktchg1$SPX.Index,Mktchg1$CN_M3,type = 'correlation', main='Chinese M3',na.action = na.pass)

Fed10 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$Fed,type = 'correlation', main='Fed reserves',na.action = na.pass, col='red')
ECB10 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$ECB,type = 'correlation', main='ECB reserves',na.action = na.pass, col='red')
BoJ10 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$BoJ,type = 'correlation', main='BoJ reserves',na.action = na.pass, col='red')
PBoC10 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$PBoC.Reserves,type = 'correlation', main='PBoC reserves',na.action = na.pass, col='red')
USM310 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$US_M3,type = 'correlation', main='US M3',na.action = na.pass, col='red')
EZM310 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$EZ_M3,type = 'correlation', main='EZ M3',na.action = na.pass, col='red')
JPM310 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$JP_M3,type = 'correlation', main='Japanese M3',na.action = na.pass, col='red')
CNM310 <- ccf(Mktchg1$TY1.Comdty,Mktchg1$CN_M3,type = 'correlation', main='Chinese M3',na.action = na.pass, col='red')

par(mfrow=c(1,1))
ResEffect <- data.frame(FedEq$acf, ECBEq$acf,BoJEq$acf,PBoCEq$acf,Fed10$acf,ECB10$acf,BoJ10$acf,PBoC10$acf)
ResEffect$Index <-seq(from=-20, to = 20, by = 1)
M3Effect <- data.frame(USM3Eq$acf, EZM3Eq$acf,JPM3Eq$acf,CNM3Eq$acf,USM310$acf,EZM310$acf,JPM310$acf,CNM310$acf)
M3Effect$Index <-seq(from=-20, to = 20, by = 1)

plot(-20:20,-ResEffect[1:41,1], type='l',col='red', xlab='Lead/Lag',ylab='CCF %', main='S&P500: effect of change in CB reserves', ylim=c(-0.6,0.6))
lines(-20:20,-ResEffect[1:41,2], type='l', col='black', lty=2)
lines(-20:20,-ResEffect[1:41,3], type='l',col='green', lty=2)
lines(-20:20,-ResEffect[1:41,4], type='l', col='blue', lty=2)
abline(h=0.1287, col='grey')
abline(h=-0.1287, col='grey')
legend(-19, 0.5, legend=c("Fed", "ECB", "BoJ", "PBoC"),
       col=c("red","black","green", "blue"), lty=c(1,2,2,2), cex=0.8)

plot(-20:20,ResEffect[1:41,5], type='l',col='red', xlab='Lead/Lag',ylab='CCF %', main='10yr T-Note future: effect of change in CB reserves', ylim=c(-0.6,0.6))
lines(-20:20,ResEffect[1:41,6], type='l', col='black', lty=2)
lines(-20:20,ResEffect[1:41,7], type='l',col='green', lty=2)
lines(-20:20,ResEffect[1:41,8], type='l', col='blue', lty=2)
abline(h=0.1287, col='grey')
abline(h=-0.1287, col='grey')
legend(-19, 0.5, legend=c("Fed", "ECB", "BoJ", "PBoC"),
       col=c("red","black","green", "blue"), lty=c(1,2,2,2), cex=0.8)

plot(-20:20,-M3Effect[1:41,1], type='l',col='red', xlab='Lead/Lag',ylab='CCF %', main='S&P500: effect of change in M3', ylim=c(-0.6,0.6))
lines(-20:20,-M3Effect[1:41,2], type='l', col='black', lty=2)
lines(-20:20,-M3Effect[1:41,3], type='l',col='green', lty=2)
lines(-20:20,-M3Effect[1:41,4], type='l', col='blue', lty=2)
abline(h=0.1287, col='grey')
abline(h=-0.1287, col='grey')
legend(-19, 0.5, legend=c("USM3", "EZM3", "JPM3", "CNM3"),
       col=c("red","black","green", "blue"), lty=c(1,2,2,2), cex=0.8)

plot(-20:20,M3Effect[1:41,5], type='l',col='red', xlab='Lead/Lag',ylab='CCF %', main='10yr T-Note future: effect of change in M3', ylim=c(-0.6,0.6))
lines(-20:20,M3Effect[1:41,6], type='l', col='black', lty=2)
lines(-20:20,M3Effect[1:41,7], type='l',col='green', lty=2)
lines(-20:20,M3Effect[1:41,8], type='l', col='blue', lty=2)
abline(h=0.1287, col='grey')
abline(h=-0.1287, col='grey')
legend(-19, 0.6, legend=c("USM3", "EZM3", "JPM3", "CNM3"),
       col=c("red","black","green", "blue"), lty=c(1,2,2,2), cex=0.8)


tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))

# Remove dates using subset
df2 <- subset(Mktchg1, select = -c(period,US2s10s_swap,USSP10.Curncy))

df2 |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
           tl.cex=0.6, cl.cex = 0.6, order = "hclust",
           hclust.method = "complete")

# convert money into USD 
ECBusd <- Alldata$ECB/Alldata$EUR.Curncy
BoJusd <- Alldata$BoJ/Alldata$JPY.Curncy
PBoCusd <- Alldata$PBoC.Reserves/Alldata$CNY.Curncy

EZM3usd <- Alldata$EZ_M3/Alldata$EUR.Curncy
JPM3usd <- Alldata$JP_M3/Alldata$JPY.Curncy/10
CNM3usd <- Alldata$CN_M3/Alldata$CNY.Curncy

Moneyusd <- data.frame(cbind('Fed'=Alldata$Fed, ECBusd,BoJusd,PBoCusd,'USM3'=Alldata$US_M3, EZM3usd,JPM3usd,CNM3usd))
#Moneyusd$period <- as.Date(ymd(Alldata$period))

Moneychg <- data.frame(apply(Moneyusd,2,function(x) diff(log(x), lag=6)))
Moneychg$period <- as.Date(Alldata$period[7:nrow(Alldata)])

Resusdchg <- (Moneychg$Fed+Moneychg$ECBusd+Moneychg$BoJusd+Moneychg$PBoCusd)
M3usdchg <- (Moneychg$USM3+Moneychg$EZM3usd+Moneychg$JPM3usd+Moneychg$CNM3usd)

par(mfrow=c(2, 2), mar=c(4,3, 4,3))  ## create a 2x2 plot

ResEq <- ccf(Mktchg1$SPX.Index,Resusdchg,type = 'correlation', main='S&P500 vs Reserves, maj CBs in USD', na.action = na.pass)
Res10 <- ccf(Mktchg1$TY1.Comdty,Resusdchg,type = 'correlation', main='10y TNote vs Reserves, maj CBs in USD',na.action = na.pass, col='red')

M3Eq <- ccf(Mktchg1$SPX.Index,M3usdchg,type = 'correlation', main='S&P500 vs M3, maj econs in USD',na.action = na.pass)
M310 <- ccf(Mktchg1$TY1.Comdty,M3usdchg,type = 'correlation', main='10y TNote vs M3, maj econs in USD',na.action = na.pass, col='red')

par(mfrow=c(1, 1), mar=c(4,3, 4,3))  ## create a 1x1 plot
plot(-20:20,-ResEq$acf, type='l',col='red', xlab='Months lead/lag',ylab='CCF %', main='Markets & global money, 6mth log change in USD, since 2003', ylim=c(-0.6,0.6))
lines(-20:20,Res10$acf, type='l', col='black', lty=2)
lines(-20:20,-M3Eq$acf, type='l',col='green', lty=2)
lines(-20:20,M310$acf, type='l', col='blue', lty=2)
abline(h=1.96/sqrt(ResEq$n.used), col='grey')
abline(h=-1.96/sqrt(ResEq$n.used), col='grey')
abline(h=0, col='black')
legend(-19, -0.2, legend=c("SP500 vs Major Reserves (inverted)", "TNote vs Major Reserves", "SP500 vs Major M3 (inverted)", "TNote vs Major M3"),
       col=c("red","black","green", "blue"), lty=c(1,2,2,2), cex=0.8)



#impulse response of selected money data on S&P500
#select US M3 & Fed reserves

AllUSD <- data.frame("period"=Moneychg$period,"M3USD"= M3usdchg,"Reserves"=Resusdchg)

ext_rge <- c(-3,3)

#add S&P500 data
mkts <- Mktchg1[,c(39,11)]
shtcomb <- left_join(AllUSD,mkts, by='period')

model <- vars::VAR(shtcomb[,-1], p = 1, type = "const")

feirResSPX <- irf(model, impulse = "Reserves", response = "SPX.Index",
               n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirResSPX)

feirM3usdSPX <- irf(model, impulse = "M3USD", response = "SPX.Index",
               n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirM3usdSPX)

feirSPXRes <- irf(model, impulse = "SPX.Index", response = "Reserves",
                  n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirSPXRes,ylim=ext_rge)

feirSPXM3usd <- irf(model, impulse = "SPX.Index", response = "M3USD",
                    n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirSPXM3usd,ylim=ext_rge)

#add TY1 data
mkts <- Mktchg1[,c(39,18)]
shtcomb[,4] <- mkts$TY1.Comdty
colnames(shtcomb)[4] <- 'TY1.Comdty'
#fill in a couple of NA 
shtcomb <- tidyr::fill(shtcomb,names(shtcomb))

model <- vars::VAR(shtcomb[,-1], p = 1, type = "const")
plot(predict(model))

feirResTY1 <- irf(model, impulse = "Reserves", response = "TY1.Comdty",
               n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirResTY1, ylim=ext_rge)

feirM3usdTY1 <- irf(model, impulse = "M3USD", response = "TY1.Comdty",
                 n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirM3usdTY1, ylim=ext_rge)

feirTY1Res <- irf(model, impulse = "TY1.Comdty", response = "Reserves",
               n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirTY1Res, ylim=ext_rge)

feirTY1M3usd <- irf(model, impulse = "TY1.Comdty", response = "M3USD",
                 n.ahead = 8, ortho = FALSE, runs = 1000)
plot(feirTY1M3usd, ylim=ext_rge)



# #select US M3 & Fed reserves
# shtcomb <-
#   Moneychg[,c(9,1,5)] %>%
#   filter(period >="2012-03-01")
# 
# #add TY1 data
# mkts <- Mktchg1[,c(39,18)]
# shtcomb <- left_join(shtcomb,mkts, by='period')
# 
# #fill in a couple of NA 
# shtcomb <- tidyr::fill(shtcomb,names(shtcomb))
# 
# # Estimate model for vector autoregression tests
# # remove date column 
# shtcomb <- shtcomb[,-1]
# model <- vars::VAR(shtcomb, p = 2, type = "const")
# 
# # Look at summary statistics
# feir <- irf(model, impulse = "Fed", response = "TY1.Comdty",
#             n.ahead = 8, ortho = FALSE, runs = 1000)
# 
# plot(feir)
# 
# feir <- irf(model, impulse = "USM3", response = "TY1.Comdty",
#             n.ahead = 8, ortho = FALSE, runs = 1000)
# 
# plot(feir)
