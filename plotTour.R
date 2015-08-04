plotTour = function(data, GAsol, plotTitle) {

  mds = cmdscale(data)
  x = mds[, 1]
  y = -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
         col = "light gray")
  tour = GAsol@solution[1, ]
  tour = c(tour, tour[1])
  n = length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
         length = 0.15, angle = 25, col = "steelblue", lwd = 2)
  text(x, y, labels(data), cex=0.8)
  title(plotTitle)

}