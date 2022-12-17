## ---------------------------
##
## Script name: quincunx
## Purpose of script: demostrate random walk through dropped balls example (quincunx). 
#  A quincunx simulates balls dropping down a pegboard with a 50% chance of bouncing right or left at each level. 
# The balls accumulate in bins. If enough balls are dropped, the distribution approaches normality. 
## Author:Robert McDonald
## Date Created: 2022-04-11
## Email: rmcd1024@gmail.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

## ---------------------------

quincunxAM <- function (n = 10, numballs = 200, delay = 0.3, probright = 0.5, 
          plottrue = TRUE) 
{
  if (exists(".Random.seed", .GlobalEnv)) {
    oldseed <- .Random.seed
    savedseed <- TRUE
  }
  else{
    savedseed <- FALSE
  }
  nlev <- n + 1
  tbl <- array(0, dim = nlev)
  names(tbl) <- 0:(n)
  x <- array(dim = nlev * (nlev + 1)/2)
  y <- array(dim = nlev * (nlev + 1)/2)
  center <- round((nlev + 0.01)/2)
  x[1] <- center
  y[1] <- nlev
  for (i in 2:nlev) {
    w <- ((i * (i - 1)/2) + 1):(i * (i + 1)/2)
    x[w] <- center + (1:i) - ((i + 1)/2)
    y[w] <- rep(nlev + 1 - i, i)
  }
  par(mfcol = c(2, 1))
  for (j in 1:numballs) {
    s <- cumsum(rbinom(nlev - 1, size = 1, prob = probright))
    tbl[s[nlev - 1] + 1] <- tbl[s[nlev - 1] + 1] + 1
    if (delay != 0 | j == numballs) {
      peglook <- function(a, b) c(rep(a, n * (n + 1)/2), 
                                  rep(b, n + 1))
      plot(x, y, axes = FALSE, main='A little demo to welcome you to the course', xlab = "Returns", ylab = "", pch = peglook(1, 
                                                                   0), cex = peglook(1, 1.25), col = peglook("black", 
                                                                                                             "red"))
      path <- c(1, cumsum(1:(nlev))[-1] - 1:(nlev - 1) + 
                  s)
      lines(x[path], y[path], lty = 2)
      h <- barplot(tbl, ylim = c(0, round(numballs/2.8)), 
                   xlab = "Number of Rightward Bounces")
      if (delay > 0) 
        Sys.sleep(delay)
      if (delay < 0) 
        readline()
    }
  }
  if (plottrue) {
    k <- 0:n
    num <- choose(n, k) * probright^k * (1 - probright)^(n - 
                                                           k) * numballs
    points(h, num, cex = 3, xlim = c(0, n), pch = 1)
  }
  if (savedseed) 
    .GlobalEnv$.Random.seed <- oldseed
}
