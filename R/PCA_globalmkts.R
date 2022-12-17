## ---------------------------
##
## Script name: PCA_globalmkts
## Purpose of script: demonstrate use of PCA to gauge relative market value 
## Author: Meyrick Chapman
## Date Created: 2022-02-20
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
library(tiff)
# source("functions/packages.R")       # loads up all the packages we need

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
    theme(legend.position="bottom")
}
last_day_prev_year <- function(x) floor_date(x, "year") - days(1)
## ---------------------------
tset <- FALSE
df<-read.csv("Data/dailyFWk1312.csv")
df$period <- as.Date(dmy(df$period))
end2021 <- last_day_prev_year(Sys.Date())
enddate <- match(end2021,df$period)-1

trainingset <- df[1:enddate,]

data_used<-xts(df[,2:ncol(df)], order.by = df$period)
period <- df$period

if(tset == TRUE) {
  data_used <-
    xts(trainingset[, 2:ncol(trainingset)], order.by = trainingset$period)
  tsperiod <- trainingset$period[1:nrow(trainingset)]
}

zdata <- scale(data_used,center= T, scale = T)
# let's look at correlation between markets since 2000
tmwr_cols <- colorRampPalette(c("#5dca22", "#CA225E"))
zdata |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
         tl.cex=0.6, cl.cex = 0.6, order = "hclust",
         hclust.method = "complete")

covarz <- cov(zdata)
tmwr_cols <- colorRampPalette(c("#fc9803", "#3503fc"))
covarz |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
           tl.cex=0.6, cl.cex = 0.6, order = "hclust",
           hclust.method = "complete")

#perform PCA on resulting complete observation set
# question: is it better to perform PCA on a moving window or the entire dataset? This version uses whole dataset.
pc1<-PCA(coredata(zdata), graph=F) #default = 5 PCAs created

fviz_contrib(pc1, choice = "var", axes = 1, top = 57)
fviz_contrib(pc1, choice = "var", axes = 2, top = 57)
fviz_contrib(pc1, choice = "var", axes = 1:5, top=57)

var <- get_pca_var(pc1)

corrplot(var$cos2, method="circle", is.corr=T)

fviz_pca_var(pc1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping,
             title="Correlation of markets to PC1&2, 2000-2022"
)

eig.val<-pc1$eig

barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by PCs (%)",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

barplot(eig.val[,3],
     names.arg = 1:nrow(eig.val), 
     main = "Cumulative variance Explained by PCs (%)",
     xlab = "Principal Components",
     ylab = "Percentage of variances",
     col ="steelblue")

eigvectors <- data.frame(pc1$svd$V)

readr::write_csv(eigvectors,"Output/V.csv")

PCs <-
  data.frame('PCA1' = c(1:nrow(zdata)), 'PCA2' = c(1:nrow(zdata)),'PCA3'= c(1:nrow(zdata)),'PCA4' = c(1:nrow(zdata)),'PCA5' = c(1:nrow(zdata)))
PCs[,] <- NA

for(row in 1:nrow(zdata)){
  PCs$PCA1[row] <- sum(eigvectors[,1]*zdata[row,])
  PCs$PCA2[row] <- sum(eigvectors[,2]*zdata[row,])
  PCs$PCA3[row] <- sum(eigvectors[,3]*zdata[row,])
  PCs$PCA4[row] <- sum(eigvectors[,4]*zdata[row,])
  PCs$PCA5[row] <- sum(eigvectors[,5]*zdata[row,])
}

plot(PCs$PCA1, type = 'l', ylab = 'Dimension value', main = 'Principal components of our universe')
lines(PCs$PCA2, col='orange')
lines(PCs$PCA3, col='red')
lines(PCs$PCA4, col='grey')

tmwr_cols <- colorRampPalette(c("#5dca22", "#CA225E"))

PCs |>  
  cor() |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", 
           tl.cex=0.6, cl.cex = 0.6, order = "hclust",
           hclust.method = "complete")

if(tset == TRUE){
readr::write_csv(PCs,"Output/PChisttrainingset.csv")
}
if(tset == FALSE){
  readr::write_csv(PCs,"Output/PChistfullset.csv")
}

fittedX <- zdata
fittedX[,] <- NA
col <- 1

for(col in 1:ncol(zdata)) {
  zdatamean <- mean(data_used[, col])
  zdatasd <- sd(data_used[, col])
  for (row in 1:nrow(zdata)) {
    fittedX[row, col] <- (
      PCs$PCA1[row] * eigvectors[col, 1] * zdatasd +
        PCs$PCA2[row] * eigvectors[col, 2] * zdatasd +
        PCs$PCA3[row] * eigvectors[col, 3] * zdatasd +
        zdatamean
    )
  }
}

plot_list = list()

for(mkt in 1:ncol(data_used)){

  df <- data.frame('Date'= period, data_used[,mkt],fittedX[,mkt])
  
    colnames(df)[3] <- paste0(names(fittedX[,mkt]),"_fitted")
  
  lngdf <-
    df %>%
    pivot_longer(c(-Date),names_to = 'key', values_to = 'value')

p <- ggstandard(lngdf,"Actual vs. fitted values, with difference", "source: Hedge Analytics", "3 years", "units")  

plot_list[[names(fittedX[,mkt])]] <- p
}

plot_list2 = list()

for(mkt in 1:ncol(data_used)){
  
  df1 <- data.frame('Date'= period, (data_used[,mkt]-fittedX[,mkt])/sd(data_used[, mkt]))
  
  colnames(df1)[2] <- paste0(names(fittedX[,mkt]),"_stddev")
  
  lngdf1 <-
    df1 %>%
    pivot_longer(c(-Date),names_to = 'key', values_to = 'value')
  
  p <- ggstandard(lngdf1,"Actual less fitted values, in standard deviation", "source: Hedge Analytics", "3 years", "standard deviation")  
  
  plot_list2[[names(fittedX[,mkt])]] <- p
}

i <- 1
for (i in 1:length(plot_list)) {
  file_name = paste0("Output/", names(plot_list[i]), "_returns.tiff")
  tiff(file_name)
  #  imageTIFF[i] <- grid::rasterGrob(tiff::readTIFF(file_name))
  print(plot_list[[i]])
  dev.off()
}

#plot_list
#plot_list2


plot_list3 = list()

for(mkt in 1:ncol(data_used)){
  
  df2 <- data.frame('Date'= period, diffinv(fittedX[2:nrow(fittedX),mkt]),diffinv(data_used[2:nrow(data_used),mkt]))
  
  colnames(df2)[2] <- paste0(names(fittedX[,mkt]),"_fitted")
  colnames(df2)[3] <- paste0(names(fittedX[,mkt]),"_actual")
  
  lngdf2 <-
    df2 %>%
    pivot_longer(c(-Date),names_to = 'key', values_to = 'value')
  
  p <- ggstandard(lngdf2,"Fitted and actual values, cumulative change", "source: Hedge Analytics", "3 years", "standard deviation")  
  
  plot_list3[[names(fittedX[,mkt])]] <- p
}

for (i in 1:length(plot_list3)) {
  file_name = paste0("Output/", names(plot_list3[i]), "_cumreturns.tiff")
  tiff(file_name)
  #  imageTIFF[i] <- grid::rasterGrob(tiff::readTIFF(file_name))
  print(plot_list3[[i]])
  dev.off()
}

