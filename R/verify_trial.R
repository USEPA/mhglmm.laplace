#' Title
#'
#' @param trial x
#' @param family x
#' @param dispersion x
#' @param size x
#'
#' @return x
#' @export
verify_trial <- function(trial, family, dispersion = 1, size = 1, n_fixed = 300, nugget = FALSE) {

  #browser()
  print(trial)

  set.seed(trial)

  zstar <- qnorm(0.95)

  # create grid
  xcoord_fixed <- runif(n_fixed)
  ycoord_fixed <- runif(n_fixed)
  x1_fixed <- rnorm(n_fixed)
  tau1_fixed <- rbinom(n_fixed, size = size, prob = 0.5)
  x1t1_fixed <- x1_fixed * tau1_fixed
  dat_fixed <- tibble(
    xcoord = xcoord_fixed,
    ycoord = ycoord_fixed,
    x1 = x1_fixed,
    tau1 = tau1_fixed,
    type = "fixed"
  )


  n_pred <- 100
  xcoord_pred_loc <- seq(0.05, 0.95, by = 0.1)
  ycoord_pred_loc <- seq(0.05, 0.95, by = 0.1)
  pred_grid <- expand.grid(xcoord_pred_loc, ycoord_pred_loc)
  xcoord_pred <- pred_grid[[1]]
  ycoord_pred <- pred_grid[[2]]
  x1_pred <- rnorm(n_pred)
  tau1_pred <- rbinom(n_pred, size = 1, prob = 0.5)
  x1t1_pred <- x1_pred * tau1_pred
  dat_pred <- tibble(
    xcoord = xcoord_pred,
    ycoord = ycoord_pred,
    x1 = x1_pred,
    tau1 = tau1_pred,
    type = "pred"
  )

  dat <- bind_rows(dat_fixed, dat_pred)
  betas <- c(beta0 = 0.5, beta1 = 0.5, beta2 = -0.5, beta3 = -0.5)
  # simulate w
  spcov_vals <- spcov_params("exponential", de = 1, ie = 1e-4, range = 1)
  dat <- dat %>%
    mutate(w = sprnorm(
      spcov_vals,
      mean = betas["beta0"] + betas["beta1"] * x1 + betas["beta2"] * tau1 + betas["beta3"] * x1 * tau1,
      data = .,
      xcoord = xcoord,
      ycoord = ycoord)
    )
  dat <- dat %>%
    mutate(y = get_response(dat, family, dispersion, size))



  dat_train <- dat %>%
    dplyr::filter(type == "fixed")
  dat_test <- dat %>%
    dplyr::filter(type == "pred")

  # modeling
  fixed_start_time <- proc.time()
  if (nugget) {
    spmod <- spglm(
      formula = y ~ x1 * tau1,
      family = eval(family),
      data = dat_train,
      spcov_type = "exponential",
      xcoord = xcoord,
      ycoord = ycoord
    )
  } else {
    spmod <- spglm(
      formula = y ~ x1 * tau1,
      family = eval(family),
      data = dat_train,
      spcov_initial = spcov_initial("exponential", ie = 0, known = "ie"),
      xcoord = xcoord,
      ycoord = ycoord
    )
  }
  fixed_end_time <- proc.time()
  fixed_time <- (fixed_end_time - fixed_start_time)[["elapsed"]]

  output_fixed <- tibble(
    trial = trial,
    beta = c("beta0", "beta1", "beta2", "beta3"),
    true = betas,
    fit = coef(spmod),
    error = true - fit,
    se_fit = sqrt(diag(vcov(spmod, var_correct = FALSE))),
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    se_fit_adj = sqrt(diag(vcov(spmod))),
    lower_fit_adj = fit - zstar * se_fit_adj,
    upper_fit_adj = fit + zstar * se_fit_adj,
    cover_fit_adj = lower_fit_adj < true & true < upper_fit_adj
  )

  preds <- predict(spmod, newdata = dat_test, se.fit = TRUE, var_correct = FALSE)
  pred_start_time <- proc.time()
  preds_adj <- predict(spmod, newdata = dat_test,  se.fit = TRUE)
  pred_end_time <- proc.time()
  pred_time <- (pred_end_time - pred_start_time)[["elapsed"]]

  output_preds <- tibble(
    trial = trial,
    true = dat_test$w,
    fit = preds$fit,
    error = true - fit,
    se_fit = preds$se.fit,
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    se_fit_adj = preds_adj$se.fit,
    lower_fit_adj = fit - zstar * se_fit_adj,
    upper_fit_adj = fit + zstar * se_fit_adj,
    cover_fit_adj = lower_fit_adj < true & true < upper_fit_adj
  )

  if (nugget) {
    nugget_char <- "nugget"
  } else {
    nugget_char <- "no-nugget"
  }
  list(fixed = output_fixed, preds = output_preds, fixed_time = fixed_time, pred_time = pred_time, family = family, nugget = nugget_char)

}
