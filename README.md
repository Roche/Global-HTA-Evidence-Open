
# Global HTA Evidence Open

This repository contains code and analysis made open source from Global HTA Evidence.

# [Rpackages](Rpackages)

This folder contains source code for R packages

## MAIC

This package facilitates performing matching-adjusted indirect comparison (MAIC) anaylsis for a disconnected treatment network where the endpoint of interest is either time-to-event (e.g. overall survival) or binary (e.g. objective tumor response).

### Documentation

https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/MAIC/docs/index.html

### Installation code
```
devtools::install_github(
  "roche/Global-HTA-Evidence-Open", 
  subdir = "Rpackages/MAIC"
  )
```

## gemtcPlus

This package implements some convinience functions for perfoming Bayesian NMA using the [gemtc package](https://github.com/gertvv/gemtc/) for standard models (binary data, hazard ratio data). It also contains some more advanced models for time-to-event data with time varying hazard ratios, such as fractional polynomial NMAs and piecewise exponential NMA models.

### examples
* [Vignette - Bayesian NMA of binary data](https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/example-nma-binary-data.html)
* [Vignette - Bayesian NMA of Hazard Ratios](https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/example-nma-hr-data.html)
* [Vignette - Bayesian NMA of survival data using Piecewise Exponential models](https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/example-nma-groupedTTE-PWE.html)
* [Vignette - Bayesian NMA of survival data using Fractional Polynomial (fixed effect model) models](https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/example-nma-groupedTTE-FP.html)
* [Vignette - Bayesian NMA of survival data using Fractional Polynomial (random effects model) models](https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/example-nma-groupedTTE-FP-RE.html)


### installation code
```
# note as of June 2020 gemtc is not available via CRAN
devtools::install_github(
  "gertvv/gemtc", subdir = "gemtc"
)

devtools::install_github(
  "roche/Global-HTA-Evidence-Open", subdir = "Rpackages/gemtcPlus"
  )
```

## rpsftmPlus

This package implements some convinience functions for working with the [rpsftm package](https://cran.r-project.org/web/packages/rpsftm/) and general analysis of trials affected by treatment switching.

### Documentation
https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/rpsftmPlus/docs/index.html

### Installation code
```
devtools::install_github(
  "roche/Global-HTA-Evidence-Open", 
  subdir = "Rpackages/rpsftmPlus"
  )
```

# [Publications](Publications)

This folder contains source code supporting published articles