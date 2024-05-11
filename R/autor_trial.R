#' Title
#'
#' @param trial x
#' @param family x
#' @param dispersion x
#' @param size x
#' @param n_fixed x
#'
#' @return x
#' @export
autor_trial <- function(trial, family, dispersion = 1, size = 1,
                        de = 1, ie = 0, range = 0.9,
                        sqrt_n_fixed = 20, neighbor_dist = 1) {

  #browser()
  print(trial)

  set.seed(trial)

  zstar <- qnorm(0.95)

  n_fixed <- sqrt_n_fixed^2
  n_miss <- 100
  xcoord <- seq_len(sqrt_n_fixed)
  ycoord <- seq_len(sqrt_n_fixed)
  coords <- expand.grid(xcoord = xcoord, ycoord = ycoord)
  W <-  1 * (as.matrix(dist(coords)) <= neighbor_dist)
  diag(W) <- 0
  W_rs <- W / rowSums(W)
  observed <- sample(c(rep(TRUE, n_fixed - n_miss), rep(FALSE, n_miss)))

  x1_fixed <- rnorm(n_fixed)
  tau1_fixed <- rbinom(n_fixed, size = size, prob = 0.5)
  x1t1_fixed <- x1_fixed * tau1_fixed
  dat <- tibble(
    xcoord = coords$xcoord,
    ycoord = coords$ycoord,
    x1 = x1_fixed,
    tau1 = tau1_fixed,
    type = "fixed",
    observed = observed
  )

  betas <- c(beta0 = 0.5, beta1 = 0.5, beta2 = -0.5, beta3 = -0.5)
  # simulate w
  spcov_vals <- spcov_params("car", de = de, ie = ie, range = range, extra = 0)
  dat <- dat %>%
    mutate(w = sprnorm(
      spcov_vals,
      mean = betas["beta0"] + betas["beta1"] * x1 + betas["beta2"] * tau1 + betas["beta3"] * x1 * tau1,
      data = .,
      xcoord = xcoord,
      ycoord = ycoord,
      W = W)
    )
  dat <- dat %>%
    mutate(y = get_response(dat, family, dispersion, size)) %>%
    mutate(y = if_else(observed, y, NA))


  current_time <- proc.time()
  if (ie == 0) {
    spmod <- spgautor(
      formula = y ~ x1 * tau1,
      family = eval(family),
      data = dat,
      spcov_type = "car",
      xcoord = xcoord,
      ycoord = ycoord,
      W = W
    )
  } else {
    spmod <- spgautor(
      formula = y ~ x1 * tau1,
      family = eval(family),
      data = dat,
      spcov_initial = spcov_initial("car", ie = NA),
      xcoord = xcoord,
      ycoord = ycoord,
      W = W
    )
  }

  fixed_time_spmodel <- (proc.time() - current_time)[["elapsed"]]

  output_fixed_spmodel <- tibble(
    trial = trial,
    beta = c("beta0", "beta1", "beta2", "beta3"),
    true = betas,
    fit = coef(spmod),
    error = true - fit,
    se_fit = sqrt(diag(vcov(spmod))),
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    software = "spmodel"
  )

  dat <- dat %>%
    mutate(id1 = seq_len(n_fixed))
    current_time <- proc.time()
    formula1 = y ~ f(id1, model = "besagproper", graph = W_rs) + x1 * tau1
    if (family == "Gamma") {
      inla_family <- "gamma"
    } else {
      inla_family <- family
    }
    m_INLA =  inla(formula1, family = eval(inla_family), data = dat,
                   control.compute = list(return.marginals.predictor = TRUE, config = TRUE),
                   control.predictor = list(compute = TRUE, link = 1),
                   safe = FALSE)
    fixed_time_INLA <- (proc.time() - current_time)[["elapsed"]]

    est_INLA <- summary(m_INLA)$fixed[1:4,'mean']
    se_INLA <-  summary(m_INLA)$fixed[1:4,'sd']



    output_fixed_INLA <- tibble(
      trial = trial,
      beta = c("beta0", "beta1", "beta2", "beta3"),
      true = betas,
      fit = est_INLA,
      error = true - fit,
      se_fit = se_INLA,
      lower_fit = fit - zstar * se_fit,
      upper_fit = fit + zstar * se_fit,
      cover_fit = lower_fit < true & true < upper_fit,
      software = "INLA"
    )

    output_fixed <- bind_rows(output_fixed_spmodel, output_fixed_INLA)

    output_fixed_time <- tibble(
      time = c(fixed_time_spmodel, fixed_time_INLA),
      software = c("spmodel", "INLA")
    )

    # prediction
    current_time <- proc.time()
    preds <- predict(spmod, se.fit = TRUE)
    preds_time_spmodel <- (proc.time() - current_time)[["elapsed"]]

    output_preds_spmodel <- tibble(
      trial = trial,
      true = dat$w[!observed],
      fit = preds$fit,
      error = true - fit,
      se_fit = preds$se.fit,
      lower_fit = fit - zstar * se_fit,
      upper_fit = fit + zstar * se_fit,
      cover_fit = lower_fit < true & true < upper_fit,
      software = "spmodel"
    )

    output_preds_INLA <- tibble(
      trial = trial,
      true = dat$w[!observed],
      fit = m_INLA$summary.linear.predictor$mean[!observed],
      error = true - fit,
      se_fit = m_INLA$summary.linear.predictor$sd[!observed],
      lower_fit = fit - zstar * se_fit,
      upper_fit = fit + zstar * se_fit,
      cover_fit = lower_fit < true & true < upper_fit,
      software = "INLA"
    )

    output_preds <- bind_rows(output_preds_spmodel, output_preds_INLA)

    output_preds_time <- tibble(
      time = c(preds_time_spmodel, 0), # INLA does prediction and estimation together
      software = c("spmodel", "INLA")
    )

    list(
      output_fixed = output_fixed,
      output_fixed_time = output_fixed_time,
      output_preds = output_preds,
      output_preds_time = output_preds_time,
      family = family,
      sqrt_n = sqrt_n_fixed
    )

}
