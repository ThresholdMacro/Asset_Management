#calculate spectral density of monetary / market waves
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(xts)
library(useful)
library(data.table)
library(GeneCycle)
# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/FlexDashboards/Framework/Data/PCA stability tests/")
data_used<-read.csv("Data/EigvalueHistory1ywindow.csv")
#results_used<-read.csv("OutData/United.States1999leadlags.csv")
#Cut out last few months because money data not updated yet
data_used<-data_used[2:(nrow(data_used)),]  
#calculate the harmonics using Genecycle library
spectral_density_all<-data.frame("Harmonics"=1:80)

par(mfrow = c(3, 2))
for(i in names(data_used)) {
  df1 <- zoo(data_used[,i])
  df1 <- na.omit(df1)
  #detrend - first by twice differences to remove trend
  df1_d2 <- zoo(diff(df1, differences = 2))
  #then by removing the seasonal component
  df1_d2d12 <- diff(df1_d2, lag = 12)
  seriesname <- names(data_used[i])
  
  plot(
    df1_d2d12,
    main = paste0(seriesname," detrended, seasonally adjusted"),
    xlab = "Months",
    ylab = expression(paste(nabla, "(", nabla ^ 2,'series', ")")),
    cex = 1.5,
    col = "red",
    font = 3
  )
  
  f.data <- data.frame(GeneCycle::periodogram(na.omit(df1_d2d12[,1])),method = "smooth")
  harmonics <- 1:80
  x<-round(f.data$freq[harmonics]*length(na.omit(df1_d2d12[,1])),0)
  y<-f.data$spec[harmonics] / sum(f.data$spec)
  spectral_series<-data.frame(y)
  names(spectral_series)<-paste0(i," spectral density")
  spectral_density_all<-cbind(spectral_density_all,spectral_series)
  plot(
    x,
    y,
    main = paste0(seriesname, " , Harmonics"),
    xlab = "Harmonics (months)",
    ylab = "Spectral Density",
    type = "h",
    cex = 1.5,
    col = "green",
    font = 3
  )
  
}

data_used$mth <-
  seq(
    from = 1,
    to = nrow(data_used)
  )

lngdata_used <-
  data_used[,c(1:3,6)] %>%
  pivot_longer(c(-mth),names_to = "key",values_to = 'value')

# Add mean, last, std dev
mu <- ddply(lngdata_used, "key", summarise, grp.mean=mean(value))
last. <-ddply(lngdata_used, "key", summarise, grp.last=last(value))
stdev. <- ddply(lngdata_used, "key", summarise, grp.sd=sd(value))
stdev.up <- data.frame('key'= stdev.$key, 'grp.sdup' = mu$grp.mean + stdev.$grp.sd)
stdev.dn <- data.frame('key'= stdev.$key, 'grp.sddn' = mu$grp.mean - stdev.$grp.sd)


p<-ggplot(lngdata_used, aes(x=value, fill=key, color=key)) +
   geom_histogram(position="identity", alpha=0.5, binwidth = 0.25)+
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
         "Level", y = "Count") +
  theme_classic()

