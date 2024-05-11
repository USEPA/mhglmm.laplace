# a function for comparing predictions for two models
plot2 <- function(d1, d2) {
  par.old = par(mar = c(5,5,1,1))
  plot(c(min(d1, d2), max(d1, d2)), c(min(d1, d2), max(d1, d2)), type = 'l',
       xlab = deparse(substitute(d1)), ylab = deparse(substitute(d2)),
       cex.lab = 2, cex.axis = 1.5)
  points(d1, d2, pch = 19)
  par(par.old)
}
