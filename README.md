
# Global HTA Evidence Open

This repository contains code and analysis made open source from Global HTA Evidence.

# Rpackages

## [MAIC](Rpackages/MAIC)

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

## [gemtcPlus](Rpackages/gemtcPlus)

This package implements some convinience functions for perfoming Bayesian NMA using the [gemtc package](https://github.com/gertvv/gemtc/) for standard models (binary data, hazard ratio data). It also contains some more advanced models for time-to-event data with time varying hazard ratios, such as fractional polynomial NMAs and piecewise exponential NMA models.

### Documentation
https://roche.github.io/Global-HTA-Evidence-Open/Rpackages/gemtcPlus/docs/index.html

### Installation code
First ensure you have installed from CRAN [gemtc package](https://cran.r-project.org/web/packages/gemtc/) and [rjags package](https://cran.r-project.org/web/packages/rjags/) and that you also have [JAGS](http://mcmc-jags.sourceforge.net/) installed.

```
devtools::install_github(
  "roche/Global-HTA-Evidence-Open", 
  subdir = "Rpackages/gemtcPlus"
  )
```

## [rpsftmPlus](Rpackages/rpsftmPlus)

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
