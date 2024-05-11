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
geostat_trial <- function(trial, family, dispersion = 1, size = 1, n_fixed = 300, sp_bayes = FALSE) {

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

  current_time <- proc.time()
  spmod <- spglm(
    formula = y ~ x1 * tau1,
    family = eval(family),
    data = dat_train,
    spcov_type = "exponential",
    xcoord = xcoord,
    ycoord = ycoord
  )
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

  #estimation


  dat_train$pos <- numFactor(dat_train$xcoord, dat_train$ycoord)
  dat_train$group <- factor(rep(1, nrow(dat_train)))

  current_time <- proc.time()
  m_glmmTMB <- glmmTMB(y ~ x1 * tau1 + exp(pos + 0 | group),
                       family = eval(family), data = dat_train)
  fixed_time_glmmTMB <- (proc.time() - current_time)[["elapsed"]]


  est_glmmTMB <- summary(m_glmmTMB)$coefficients$cond[1:4,'Estimate']
  se_glmmTMB <- summary(m_glmmTMB)$coefficients$cond[1:4,'Std. Error']

  output_fixed_glmmTMB <- tibble(
    trial = trial,
    beta = c("beta0", "beta1", "beta2", "beta3"),
    true = betas,
    fit = est_glmmTMB,
    error = true - fit,
    se_fit = se_glmmTMB,
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    software = "glmmTMB"
  )

  output_fixed <- bind_rows(output_fixed_spmodel, output_fixed_glmmTMB)

  output_fixed_time <- tibble(
    time = c(fixed_time_spmodel, fixed_time_glmmTMB),
    software = c("spmodel", "glmmTMB")
  )

  # prediction
  current_time <- proc.time()
  preds <- predict(spmod, newdata = dat_test,  se.fit = TRUE)
  preds_time_spmodel <- (proc.time() - current_time)[["elapsed"]]

  output_preds_spmodel <- tibble(
    trial = trial,
    true = dat_test$w,
    fit = preds$fit,
    error = true - fit,
    se_fit = preds$se.fit,
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    software = "spmodel"
  )

  dat_test$pos <- numFactor(dat_test$xcoord, dat_test$ycoord)
  dat_test$group <- factor(rep(1, nrow(dat_test)))
  current_time <- proc.time()
  p_glmmTMB <-  predict(m_glmmTMB, newdata=dat_test, allow.new.levels = TRUE, se.fit = TRUE,
                      type = 'link')
  preds_time_glmmTMB <- (proc.time() - current_time)[["elapsed"]]

  pred_glmmTMB <-  p_glmmTMB$fit
  sepred_glmmTMB <-  p_glmmTMB$se.fit

  output_preds_glmmTMB <- tibble(
    trial = trial,
    true = dat_test$w,
    fit = p_glmmTMB$fit,
    error = true - fit,
    se_fit = p_glmmTMB$se.fit,
    lower_fit = fit - zstar * se_fit,
    upper_fit = fit + zstar * se_fit,
    cover_fit = lower_fit < true & true < upper_fit,
    software = "glmmTMB"
  )

  output_preds <- bind_rows(output_preds_spmodel, output_preds_glmmTMB)

  output_preds_time <- tibble(
    time = c(preds_time_spmodel, preds_time_glmmTMB),
    software = c("spmodel", "glmmTMB")
  )


  if (sp_bayes) {
    fit <- glm(y ~ x1 * tau1, family = eval(family), data = dat_train)
    beta.starting <- coefficients(fit)
    beta.tuning <- t(chol(vcov(fit)))
    n.batch <- 200
    # n.batch <- 500
    batch.length <- 50
    n.samples <- n.batch * batch.length

    current_time <- proc.time()
    m_spBayes <- spGLM(y ~ x1 * tau1, data = dat_train, family = eval(family),
                       coords = as.matrix(cbind(dat_train$xcoord, dat_train$ycoord)),
                       starting = list("beta" = beta.starting, "phi" = 0.06, "sigma.sq" = 1, "w" = 0),
                       tuning=list("beta" = beta.tuning, "phi" = 0.5, "sigma.sq" = 0.5, "w" = 0.5),
                       priors=list("beta.Normal" = list(c(0,0,0,0), c(10,10,10,10)),
                                   "phi.Unif" = c(0.03, 5), "sigma.sq.IG" = c(2, 1)),
                       amcmc = list("n.batch" = n.batch, "batch.length" = batch.length,
                                    "accept.rate" = 0.43),
                       cov.model="exponential", n.report=10, verbose = FALSE)
    fixed_time_spBayes <- (proc.time() - current_time)[["elapsed"]]
    spBayes_beta_theta_wburnin <- m_spBayes$p.beta.theta.samples
    spBayes_beta_theta <- spBayes_beta_theta_wburnin[5001:10000, ]
    # spBayes_beta_theta = spBayes_beta_theta_wburnin[12501:25000,]

    int_est <- mean(spBayes_beta_theta[,"(Intercept)"])
    int_se <- sqrt(var(spBayes_beta_theta[,"(Intercept)"]))
    x1_est <- mean(spBayes_beta_theta[,"x1"])
    x1_se <- sqrt(var(spBayes_beta_theta[,"x1"]))
    tau1_est <- mean(spBayes_beta_theta[,"tau1"])
    tau1_se <- sqrt(var(spBayes_beta_theta[,"tau1"]))
    x1tau1_est <- mean(spBayes_beta_theta[,"x1:tau1"])
    x1tau1_se <- sqrt(var(spBayes_beta_theta[,"x1:tau1"]))

    spBayes_fit <-  c(int_est, x1_est, tau1_est, x1tau1_est)
    spBayes_se <- c(int_se, x1_se, tau1_se, x1tau1_se)

    output_fixed_spBayes <- tibble(
      trial = trial,
      beta = c("beta0", "beta1", "beta2", "beta3"),
      true = betas,
      fit = spBayes_fit,
      error = true - fit,
      se_fit = spBayes_se,
      lower_fit = fit - zstar * se_fit,
      upper_fit = fit + zstar * se_fit,
      cover_fit = lower_fit < true & true < upper_fit,
      software = "spBayes"
    )
    output_fixed <- bind_rows(output_fixed, output_fixed_spBayes)
    output_fixed_time <- bind_rows(output_fixed_time, tibble(time = fixed_time_spBayes, software = "spBayes"))

    # prediction

    Xp <- model.matrix(~ x1 * tau1, data = dat_test)

    current_time <- proc.time()
    p_spBayes = spPredict(m_spBayes, pred.covars=Xp,
                          pred.coords=as.matrix(cbind(dat_test$xcoord, dat_test$ycoord)),
                          start=0.5*n.samples, verbose = FALSE)
    preds_time_spBayes <- (proc.time() - current_time)[["elapsed"]]

    pred_spBayes <- apply(log(p_spBayes$p.y.predictive.samples), 1, mean)
    sepred_spBayes <- sqrt(apply(log(p_spBayes$p.y.predictive.samples), 1, var))

    output_preds_spBayes <- tibble(
      trial = trial,
      true = dat_test$w,
      fit = pred_spBayes,
      error = true - fit,
      se_fit = sepred_spBayes,
      lower_fit = fit - zstar * se_fit,
      upper_fit = fit + zstar * se_fit,
      cover_fit = lower_fit < true & true < upper_fit,
      software = "spBayes"
    )

    output_preds <- bind_rows(output_preds, output_preds_spBayes)
    output_preds_time <- bind_rows(output_preds_time, tibble(time = preds_time_spBayes, software = "spBayes"))

  }

  list(
    output_fixed = output_fixed,
    output_fixed_time = output_fixed_time,
    output_preds = output_preds,
    output_preds_time = output_preds_time,
    family = family,
    n = n_fixed
  )

}
