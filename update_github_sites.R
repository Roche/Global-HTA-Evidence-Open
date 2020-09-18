# script to update the documentation and vignettes hosted at https://roche.github.io/Global-HTA-Evidence-Open/index.html
# for each package once a change is made update the following files then run the appropriate section of code
# this will pre-compile all the vignettes and documentation to the website

##############################################
# Updates to add a new package
# when adding a new package is necessary to include links in the following files as well as this script:
# README.md 
# docs/index.md

##############################################
# rpsftmPlus
##############################################
# 1) make any updates to the package 
# 2) update documentation using devtools
devtools::document(pkg = "Rpackages/rpsftmPlus")

# 3) update the file Rpackages/rpsftmPlus/_pkgdown.yml
# 4) rebuild documentation using pkgdown
pkgdown::build_site(pkg = "Rpackages/rpsftmPlus")

# 5) copy site to correct location in repo for github sites hosting
file.copy(from = "Rpackages/rpsftmPlus/docs", 
          to = "docs/Rpackages/rpsftmPlus", 
          overwrite = TRUE, 
          recursive = TRUE)

##############################################
# MAIC
##############################################
# 1) make any updates to the package 
# 2) update documentation using devtools
devtools::document(pkg = "Rpackages/MAIC")

# 3) update the file Rpackages/MAIC/_pkgdown.yml
# 4) rebuild documentation using pkgdown
pkgdown::build_site(pkg = "Rpackages/MAIC")

# 5) copy site to correct location in repo for github sites hosting
file.copy(from = "Rpackages/MAIC/docs", 
          to = "docs/Rpackages/MAIC", 
          overwrite = TRUE, 
          recursive = TRUE)

