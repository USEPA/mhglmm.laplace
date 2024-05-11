#' @export
logit <- function(x) {
  log(x / (1 - x))
}

#' @export
expit <- function(x) {
  exp(x) / (1 + exp(x))
}
