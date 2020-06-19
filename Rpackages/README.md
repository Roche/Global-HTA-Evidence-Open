
# Rpackages

This folder contains source code for R-packages.


## gemtcPlus

This package implements some convinience functions for perfoming Bayesian NMA using the [gemtc package](https://github.com/gertvv/gemtc/). 

```
# installation code
# note as of June 2020 gemtc is not available via CRAN

devtools::install_github(
  "gertvv/gemtc", subdir = "gemtc"
)

devtools::install_github(
  "roche/Global-HTA-Evidence-Open", subdir = "/Rpackages/gemtcPlus"
  )
```

## rpsftmPlus

This package implements some convinience functions for working with the [rpsftm package](https://cran.r-project.org/web/packages/rpsftm/) and general analysis of trials affected by treatment switching. [Vignette](rpsftmPlus/inst/doc/rpsftmPlus-vignette.pdf).

```
#installation code
devtools::install_github(
  "roche/Global-HTA-Evidence-Open", subdir = "/Rpackages/rpsftmPlus"
  )
```

