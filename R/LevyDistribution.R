## ---------------------------
##
## Script name: Levy distribution
## Purpose of script:
## Author: Meyrick Chapman
## Date Created: 2022-12-12
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
library(purrr)

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

x = seq(-4, 4, by = 0.01)
beta = seq(0, 1, by = 0.25) %>% setNames(., .)
data = map_dfr(
beta, .id = 'beta',
~ list(x = x, y = stabledist::dstable(x, 0.5, .x, 1, pm = 1)),
)
pretty_label = function (beta) {
bquote(beta == .(sprintf('%0.02f', as.numeric(beta))))
}
ggplot(data) +
aes(x, y, color = beta) +
geom_path() +
scale_color_discrete('', labels = function (b) map(b, pretty_label))
