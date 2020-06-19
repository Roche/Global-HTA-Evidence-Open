# Read in example data, transform, and create a mtc.result
data("hr_data")

rsd <- 834100

jags_inits <- list(
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 94387), # for reproducible jags runs
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 24507),
  list(".RNG.name"="base::Wichmann-Hill", ".RNG.seed" = 39483)
)


labels <- unique(hr_data$ref)


darm2 <- dplyr::transmute(hr_data,
  study,
  treatment = new,
  diff = lhr,
  std.err = lhrse
)
darm1 <- dplyr::transmute(hr_data,
  study,
  treatment = ref,
  diff = NA,
  std.err = NA
)

dmtc <- rbind(darm1, darm2)
nw <- gemtc::mtc.network(data.re = dmtc)

n.chains <- 3
set.seed(rsd)
models <- list(
               fixed.effect =  gemtc::mtc.model(
                                                nw,
                                                n.chain = n.chains,
                                                likelihood = "normal",
                                                link = "identity",
                                                linearModel = "fixed",
                                                om.scale = 5
              )
)

for(j in seq_along(n.chains)){
  inits_ij <- models$fixed.effect$inits[[j]]
  models$fixed.effect$inits[[j]] <- c(jags_inits[[j]], inits_ij)
}

n.iter <- 10000
n.burnin <- 5000
n.thin <- 1


mtc.result.output <- gemtc::mtc.run(models$fixed.effect,
                                    n.adapt = n.burnin,
                                    n.iter = n.iter,
                                    thin = n.thin)
saveRDS(object = mtc.result.output,
        file = "tests/data/mtcResultOutput.RDS")


