library(spBayes)
library(MBA)
library(coda)
library(spmodel)
library(glmmTMB)
library(xtable)
library(viridis)
library(classInt)
library(mhglmm.laplace)

################################################################################
################################################################################
################################################################################
##         Small example, 156 observed, 100 predicted
################################################################################
################################################################################
################################################################################

set.seed(1125)
ngrids = 16^2
betas = c(0.5, 0.5, -0.5, 0.5)
gammas = c(1, 3, 0.0001)
sim_data = simSGLM_wExpl(ngrids, autocor_fun = rho_exp,
		betas = betas, gammas = gammas,
		loc_type = 'grid', family = 'poisson', pred = FALSE)
# create 100 unique holdout indices, where the first two indices
# are the middle site and the upper corner site
predindex = sample(1:ngrids, 100)[1:100]
sim_data[predindex,'obspred'] = 'pred'
coords = sim_data[sim_data$obspred == 'obs',c('xcoord','ycoord')]
distmat = as.matrix(dist(coords))
xypred = sim_data[predindex,c('xcoord','ycoord')]
npred = dim(xypred)[1]
n = dim(coords)[1]
DF = sim_data[sim_data$obspred == 'obs',]
DFp = sim_data[predindex,]
DFna = sim_data
DFna[DFna$obspred == 'pred','y'] = NA

################################
#
## spmodel
#
################################

# estimation

current_time <- proc.time()
m_spmodel <- spglm(y ~ x_1*x_2, family = poisson, DF,
	spcov_initial = spcov_initial("exponential", ie = 0, known = 'ie'),
	xcoord = xcoord, ycoord = ycoord)
spmodel_time <- proc.time() - current_time
summary(m_spmodel)

est_spmodel = summary(m_spmodel)$coefficients$fixed[2:4,'estimates']
se_spmodel = summary(m_spmodel)$coefficients$fixed[2:4,'Std_Error']
theta_spmodel = coef(m_spmodel, type = 'spcov')[c('de','range')]

# prediction

current_time <- proc.time()
p_spmodel = predict(m_spmodel, DFp, se.fit = TRUE)
spmodel_ptime <- proc.time() - current_time

pred_spmodel = p_spmodel$fit
sepred_spmodel = p_spmodel$se.fit

################################
#
## spBayes
#
################################

#estimation

fit <- glm(y ~ x_1*x_2, family="poisson", data = DF)
beta.starting <- coefficients(fit)
beta.tuning <- t(chol(vcov(fit)))
n.batch <- 400
batch.length <- 50
n.samples <- n.batch*batch.length

current_time <- proc.time()
m_spBayes <- spGLM(y ~ x_1*x_2, data = DF, family="poisson",
	coords = as.matrix(coords),
  starting = list("beta" = beta.starting, "phi" = 0.06, "sigma.sq" = 1, "w" = 0),
  tuning=list("beta" = beta.tuning, "phi" = 0.5, "sigma.sq" = 0.5, "w" = 0.5),
  priors=list("beta.Normal" = list(c(0,0,0,0), c(10,10,10,10)),
		"phi.Unif" = c(0.03, 5), "sigma.sq.IG" = c(2, 1)),
  amcmc = list("n.batch" = n.batch, "batch.length" = batch.length,
		"accept.rate" = 0.43),
  cov.model="exponential", verbose=FALSE, n.report=10)
spBayes_time = proc.time() - current_time
spBayes_beta_theta_wburnin = m_spBayes$p.beta.theta.samples
spBayes_beta_theta = spBayes_beta_theta_wburnin[10001:20000,]

betahat_x1_spBayes = mean(spBayes_beta_theta[,"x_1"])
betahat_x2_spBayes = mean(spBayes_beta_theta[,"x_21"])
betahat_x1x2_spBayes = mean(spBayes_beta_theta[,"x_1:x_21"])
betahat_x1se_spBayes = sqrt(var(spBayes_beta_theta[,"x_1"]))
betahat_x2se_spBayes = sqrt(var(spBayes_beta_theta[,"x_21"]))
betahat_x1x2se_spBayes = sqrt(var(spBayes_beta_theta[,"x_1:x_21"]))

est_spBayes = c(betahat_x1_spBayes, betahat_x2_spBayes, betahat_x1x2_spBayes)
se_spBayes = c(betahat_x1se_spBayes, betahat_x2se_spBayes,
	betahat_x1x2se_spBayes)

# prediction

Xp = model.matrix(~ x_1*x_2, data = DFp)

current_time <- proc.time()
p_spBayes = spPredict(m_spBayes, pred.covars=Xp,
	pred.coords=as.matrix(xypred),
	start=0.5*n.samples)
spBayes_ptime <- proc.time() - current_time

pred_spBayes = apply(log(p_spBayes$p.y.predictive.samples),1,mean)
sepred_spBayes = sqrt(apply(log(p_spBayes$p.y.predictive.samples),1,var))

# MAP estimate of autocorrelation parameter
# spBayes parameterizes as exp(-phi*distance)
# more usual is exp(-distance/phi) so compute posterior of 1/phi
# find the mode of the posterior distribution
post_phi = density(1/spBayes_beta_theta[,"phi"])
post_phi$x[
	which(post_phi$y == max(post_phi$y))
]
post_sigma2 = density(spBayes_beta_theta[,"sigma.sq"], bw = 0.03)
post_sigma2$x[
	which(post_sigma2$y == max(post_sigma2$y))
]
# store the results in a 2-element vector
sigma2_spBayes = post_sigma2$x[which(post_sigma2$y == max(post_sigma2$y))]
phi_spBayes = post_phi$x[which(post_phi$y == max(post_phi$y))]
theta_spBayes = c(sigma2_spBayes, phi_spBayes)


################################
#
## glmmTMB
#
################################

#estimation

DF$pos <- numFactor(DF$xcoord, DF$ycoord)
DF$group <- factor(rep(1, nrow(DF)))

current_time <- proc.time()
m_TMB <- glmmTMB(y ~ x_1*x_2 + exp(pos + 0 | group), family = poisson, data = DF)
TMB_time <- proc.time() - current_time
summary(m_TMB)
# covariance structures given here
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
# variance
(exp(m_TMB$sdr$par.fixed[5]))^2
# autocorrelation
exp(m_TMB$sdr$par.fixed[6])
covMat = VarCorr(m_TMB)[[1]][['group']]
diag(covMat)
# to verify autocorrelation parameter as parameterized by spmodel
# covMat[1,2] 0.28125 - 0.21875 = 0.0625 units apart because same x-coordinate
# (see coords for simulated data)
# an estimate of sigma^2 is given by the diagonal of covMat
covMat[1,1]
# so covMat[1,2] = covMat[1,1]*exp(-0.0625/rho) or
# rho = -0.0625/log(covMat[1,2]/covMat[1,1])
-0.0625/log(covMat[1,2]/covMat[1,1])
# store the results in a 2-element vector
theta_glmmTMB = c((exp(m_TMB$sdr$par.fixed[5]))^2,
	exp(m_TMB$sdr$par.fixed[6]))


est_glmmTMB = summary(m_TMB)$coefficients$cond[2:4,'Estimate']
se_glmmTMB = summary(m_TMB)$coefficients$cond[2:4,'Std. Error']

DFp$pos <- numFactor(DFp$xcoord, DFp$ycoord)
DFp$group <- factor(rep(1, nrow(DFp)))

# prediction

current_time <- proc.time()
p_TMB = predict(m_TMB, newdata=DFp, allow.new.levels=TRUE, se.fit = TRUE,
	type = 'link')
TMB_ptime <- proc.time() - current_time

pred_glmmTMB = p_TMB$fit
sepred_glmmTMB = p_TMB$se.fit

################################
#
## Table 2
#
################################

est_outDF = data.frame(True = betas[2:4],
	est_spmodel = est_spmodel,
	est_spBayes = est_spBayes,
	est_glmmTMB = est_glmmTMB,
	se_spmodel = se_spmodel,
	se_spBayes = se_spBayes,
	se_glmmTMB = se_glmmTMB)
print(
    xtable(est_outDF,
      align = c('l',rep('l', times = length(est_outDF[1,]))),
      digits = c(1,1,3,3,3,3,3,3),
    ),
    size = 'footnotesize',
    sanitize.text.function = identity,
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
)




