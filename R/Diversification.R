## ---------------------------
##
## Script name: Diversification
## Purpose of script: show effect of diversification on risk of portfolio
## Author: Meyrick Chapman
## Date Created: 2022-11-25
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   original here: https://www.r-bloggers.com/2019/07/back-to-diversification/
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(tidyquant)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(cowplot)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------  


# Create toy portfolio
set.seed(123)
mu <- seq(-.03/12,.08/12,.001)
sigma <- seq(0.02, 0.065, .005)

mat <- matrix(nrow = 60, ncol = 10)
for(i in 1:ncol(mat)){
  mu_samp <- sample(mu, 1, replace = FALSE)
  sig_samp <- sample(sigma, 1, replace = FALSE)
  mat[,i] <- rnorm(nrow(mat), mu_samp, sig_samp)
}

df <- as.data.frame(mat)
asset_names <- toupper(letters[1:10])
colnames(df) <- asset_names

# Cumulative returns 
df_comp <- rbind(rep(1,10), cumprod(df+1))

# Return graph
ret_graph <- df_comp %>% 
  mutate(date = 0:60) %>%
  gather(key, value, -date) %>%
  ggplot(aes(date, (value-1)*100, color = key)) +
  geom_line() +
  labs(y = "Return (%)",
       x = "Month",
       title ="Cumulative returns for random stock sample") + 
  theme(legend.position = "none",
        plot.title = element_text(size = 10))

# Create volatility date frame
vol <- df %>% summarise_all(., sd) %>% t() %>% as.numeric()
vol <- data.frame(asset = asset_names, vol = vol)


# Portfolio volatility
weights <- rep(0.1, 10)
port_vol <- sqrt(t(weights) %*% cov(df) %*% weights)
# round(port_vol*sqrt(12), 3)*100

# volatility graph
vol_graph <- vol %>% 
  mutate(vol = vol*sqrt(12)*100) %>%
  ggplot(aes(reorder(asset,vol), vol)) + 
  geom_bar(stat = "identity", fill = "royalblue1") + 
  geom_hline(yintercept = port_vol*sqrt(12)*100, color = "red") + 
  labs(y = "Volatility (%)",
       x = "",
       title = "Stock volatility with portfolio volatility line") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10)) +
  annotate("text", x = 3, 
           y = 10, 
           label = "Portfolio volatility", 
           color = "red",
           size = 3.5)

# Plot side by side graphs of returns and volatlities
plot_grid(ret_graph, vol_graph)

## Create random correlations
set.seed(123)
corr_list <- list()
combo <- expand.grid(vol$vol, vol$vol)
combo <- combo %>% mutate(Var3 = Var1 * Var2)
for(i in 1:10000){
  test_mat <- matrix(nrow = 10, ncol = 10)
  rand_cor <- runif(45, -1, 1)
  test_mat[upper.tri(test_mat, diag = FALSE)] <- rand_cor
  test_mat[lower.tri(test_mat, diag = FALSE)] <- rand_cor
  test_mat[is.na(test_mat)] <- 1
  cov_mat <- test_mat * combo$Var3   
  corr_list[[i]] <- cov_mat
}

# Calculate portfolio volatility
rand_vol <- c()
for(i in 1:10000){
  rand_vol[i] <- sqrt(t(weights) %*% corr_list[[i]] %*% weights)
}

# NaNs produced since some correlations produced randomly are impossible

# Create data frame and graph
rand_vol_df <- data.frame(vol = na.omit(rand_vol))

rand_vol_df %>% 
  ggplot(aes(vol*sqrt(12)*100)) + 
  geom_histogram(bins = 200, fill = "slateblue1") +
  geom_vline(xintercept = port_vol*sqrt(12)*100, color = "red", lwd = 1.5) +
  geom_vline(xintercept = min(vol$vol) * sqrt(12) * 100, color = "purple", lwd = 1.2) + 
  # geom_vline(xintercept = mean_vol_port, lwd = 1.2) +
  # geom_density(aes(y = .01*..count..), color = "blue") +
  labs(x = "Volatility (%)", y = "Count", 
       title = "Portfolio volatility distribution based on random correlation sample") +
  annotate("text", x = 2.2, y = 100, 
           label = "Original portfolio volatility", 
           color = "red", size = 3.5) +
  annotate("text", x = 7.5, y = 100, 
           label = "Lowest stock volatility", 
           color = "blue", size = 3.5)
