function (R, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
          width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
          gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
          p = 0.95, plot.engine = "default", ...) 
{
  begin = begin[1]
  x = checkData(R)
  colnames = colnames(x)
  ncols = ncol(x)
  if (plot.engine != "default" && plot.engine != "ggplot2" && 
      plot.engine != "plotly") {
    warning("Please use correct arguments:\n              \"default\",\"ggplot2\",\"plotly\".\n              \n              Ploting chart using built-in engine now.")
    plot.engine = "default"
  }
  length.column.one = length(x[, 1])
  start.row = 1
  start.index = 0
  while (is.na(x[start.row, 1])) {
    start.row = start.row + 1
  }
  x = x[start.row:length.column.one, ]
  if (ncols > 1) 
    legend.loc = legend.loc
  else legend.loc = NULL
  if (is.null(main)) 
    main = paste(colnames[1], "Performance", sep = " ")
  if (ylog) 
    wealth.index = TRUE
  op <- par(no.readonly = TRUE)
  par(oma = c(2, 0, 4, 0), mar = c(1, 4, 4, 2), bg='grey')
  par(mar = c(1, 4, 0, 2))
  freq = periodicity(x)
  switch(freq$scale, seconds = {
    date.label = "Second"
  }, minute = {
    date.label = "Minute"
  }, hourly = {
    date.label = "Hourly"
  }, daily = {
    date.label = "Daily"
  }, weekly = {
    date.label = "Weekly"
  }, monthly = {
    date.label = "Monthly"
  }, quarterly = {
    date.label = "Quarterly"
  }, yearly = {
    date.label = "Annual"
  })
  par(mar = c(5, 4, 0, 2))
  switch(plot.engine, default = {
    plot_object <- chart.CumReturns(x, main = "Cumulative Return", 
                                    xaxis = FALSE, legend.loc = legend.loc, event.labels = event.labels, 
                                    ylog = ylog, wealth.index = wealth.index, begin = begin, 
                                    geometric = geometric, ylab = "Cumulative Return", 
                                    plot.engine = "default", ...)
    plot_object <- chart.BarVaR(x, main = paste(date.label, 
                                                "Return"), xaxis = FALSE, width = width, ylab = paste(date.label, 
                                                                                                      "Return"), methods = methods, event.labels = NULL, 
                                ylog = FALSE, gap = gap, p = p, add = TRUE, plot.engine = "default", 
                                ...)
    plot_object <- chart.Drawdown(x, geometric = geometric, 
                                  main = "Drawdown", ylab = "Drawdown", event.labels = NULL, 
                                  ylog = FALSE, add = TRUE, plot.engine = "default", 
                                  ...)
    print(plot_object)
    title(main, outer = TRUE)
    par(op)
  }, ggplot2 = {
    plot_object_CumReturn <- chart.CumReturns(x, main = "Cumulative Return", 
                                              xaxis = FALSE, legend.loc = legend.loc, event.labels = event.labels, 
                                              ylog = ylog, wealth.index = wealth.index, begin = begin, 
                                              geometric = geometric, ylab = "Cumulative Return", 
                                              plot.engine = "ggplot2", ...)
    plot_object_Drawdown <- chart.Drawdown(x, geometric = geometric, 
                                           main = "Drawdown", ylab = "Drawdown", event.labels = NULL, 
                                           ylog = FALSE, add = TRUE, plot.engine = "ggplot2", 
                                           ...)
    plot_object <- gridExtra::grid.arrange(plot_object_CumReturn, 
                                           plot_object_Drawdown, nrow = 2)
    return(plot_object)
  }, plotly = {
    plot_object_CumReturn <- chart.CumReturns(x, xaxis = FALSE, 
                                              legend.loc = legend.loc, event.labels = event.labels, 
                                              ylog = ylog, wealth.index = wealth.index, begin = begin, 
                                              geometric = geometric, ylab = "Cumulative Return", 
                                              plot.engine = "plotly", ...)
    plot_object_Drawdown <- chart.Drawdown(x, geometric = geometric, 
                                           ylab = "Drawdown", event.labels = NULL, ylog = FALSE, 
                                           add = TRUE, plot.engine = "plotly", ...)
    plot_object = plotly::subplot(plot_object_CumReturn, 
                                  plot_object_Drawdown, nrows = 2)
    return(plot_object)
  })
}
