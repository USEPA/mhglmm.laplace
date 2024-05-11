get_response <- function(data, family, dispersion, size) {
  n <- NROW(data)
  if (family == "poisson") {
    y <- rpois(n = n, lambda = exp(data$w))
  } else if (family == "nbinomial") {
    y <- rnbinom(n = n, mu = exp(data$w), size = dispersion)
  } else if (family == "binomial") {
    y <- rbinom(n, size, expit(data$w))
  } else if (family == "beta") {
    mu <- expit(data$w)
    a <- mu * dispersion
    b <- (1 - mu) * dispersion
    y <- rbeta(n, shape1 = a, shape2 = b)
    y <- pmax(1e-6, y)
    y <- pmin(1 - 1e-6, y)
  } else if (family == "Gamma") {
    mu <- exp(data$w)
    y <- rgamma(n = n, shape = dispersion, scale = mu / dispersion)
  } else if (family == "inverse.gaussian") {
    mu <- exp(data$w)
    y <- rinvgauss(n, mean = mu, dispersion = 1 / (mu * dispersion))
    # y <- rinvgauss(n, mean = mu, shape = mu * dispersion)
  } else {
    stop("Invalid family")
  }
  y
}
