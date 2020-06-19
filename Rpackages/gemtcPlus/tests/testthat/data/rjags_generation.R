library(R2jags)
library(ggmcmc)
devtools::load_all()

rsd <- 29348
do.conv.diag <- TRUE
# load data
data("grouped_KM")


# Run the NMA

## NMA input data

ref.std <- "STUDY2"                # select study (for baseline estimates)
nma.ref.trt <- "B"                 # need to select well connected treatment as nw ref


dat <- grouped_KM %>%
        mutate(studyf = relevel(study, ref = ref.std),
               treatmentf = relevel(treatment, ref = nma.ref.trt),
               studyn = as.numeric(studyf),
               treatmentn = as.numeric(treatmentf),
               dt = t.end - t.start)


cut.points <- c(3, 10)      # piecewise constant model with two cutpoints

cut.points0 <- c(0, cut.points)
cut.pointsInf <- c(cut.points, Inf)

Nobs <- nrow(dat)
dat$segment <- rep(NA, Nobs)
for (i in 1:Nobs){
  dat$segment[i] <- last(which(dat$t.start[i] >= cut.points0))
  if (dat$t.end[i] > cut.pointsInf[dat$segment[i]]) { warning(paste("Data line", i, "spans more than 1 segment"))}
}


# Prepare input data: counters for arm etc
d_arms <- dat %>%
  group_by(study, treatment) %>%
  slice(1) %>%
  group_by(study) %>%
  mutate(arm = 1:n(), n_arms = max(arm)) %>%
  select(study, treatment, studyn, treatmentn, arm, n_arms)
d_arms

dat <- dat %>%
  left_join(d_arms, by = c("study", "treatment", "studyn", "treatmentn"))

d_std <- d_arms %>%
  group_by(studyn) %>%
  select(studyn, n_arms) %>%
  slice(1)
d_std

d_trts <- dat %>%
  mutate(studyn.arm = interaction(studyn, arm)) %>%
  filter(!duplicated(studyn.arm)) %>%
  select(studyn, arm, treatmentn) %>%
  arrange(studyn, arm) %>%
  tidyr::spread(key = arm, treatmentn, drop = FALSE) # identify trt in each am for each study
d_trts




dat_jg <- list(
  Nobs = Nobs,
  Ns = nrow(d_std),
  Na = d_std$n_arms,
  segment = dat$segment,
  Ncuts = length(cut.points),
  r = dat$n.event,
  n = dat$n.risk,
  dt = dat$dt,
  s = dat$studyn,
  a = dat$arm,
  t = as.matrix(select(ungroup(d_trts), -studyn)),
  Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
)
dat_jg


## JAGS fits
## output object needed for each fit
out_gen <- list(r.seed = rsd,
                data.jg = dat_jg,
                data.df = dat,
                data.arms = d_arms,
                model.type = "PWE",
                model.pars = list(cut.pts = cut.points))
out_all <- list()

fit.count <- 0


### Fixed effect model

n.iter <- 20000
n.burnin <- 10000
n.thin <- 1
n.chains <- 3


# JAGS fit
set.seed(rsd)
fit <- jags(model.file = system.file("R", "tte_piece-wise-cst_fe.txt", package = "gemtcPlus"),
            data = c(dat_jg,
                     list(prior_mean = 0),
                     list(prior_prec = 0.0001)),
            parameters = c("d", "mu"),
            n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin)
fit.count <- fit.count + 1

# Re-calculate DIC as default in JAGS uses normal approximation instead of full MCMC samples
DICsamp <- dic.samples(fit$model, n.iter = n.iter, n.burn = n.burnin, thin = n.thin, type="pD")
DICsamp

fit$BUGSoutput$DIC      <- sum(DICsamp$deviance) + sum(DICsamp$penalty)
fit$BUGSoutput$pD       <- sum(DICsamp$penalty)
fit$BUGSoutput$DICbyR   <- FALSE

# save single fit object
saveRDS(object = fit, file = "tests/data/rjags_output.RDS")

# save list of fits (only one fit for minimalism) which is the input for some functions we need to test
fit_out <- list()
fit_out[[1]] <- c(out_gen,
                          list(fit = fit,
                               DICsamp = DICsamp,
                               descr_s = "PWE, FE",
                               descr = "Piecewise exponential model (fixed effect)",
                               RE = FALSE,
                               comment.re = NA,
                               prior = NA)
  )
saveRDS(object = fit_out, file = "tests/data/rjags_output_list.RDS")
