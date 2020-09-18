# script to update the documentation and vignettes hosted at https://roche.github.io/Global-HTA-Evidence-Open/index.html
# for each package once a change is made update the following files then run the appropriate section of code
# this will pre-compile all the vignettes and documentation to the website

##############################################
# Updates to add a new package
# when adding a new package is necessary to include links/make updates in the following files as well as in this script:
# README.md 
# all existing _pkgdown.yml files (update the navbar section for other packages)

# update the index page (just converts the README.md to html)

markdown::markdownToHTML("README.md", output = "docs/index.html")

##############################################
# rpsftmPlus
##############################################
# 1) make any updates to the package 
# 2) update documentation using devtools
devtools::document(pkg = "Rpackages/rpsftmPlus")

# 3) reinstall the updated package 
devtools::install(pkg = "Rpackages/rpsftmPlus")

# 4) update the file Rpackages/rpsftmPlus/_pkgdown.yml
# 5) rebuild documentation using pkgdown
pkgdown::build_site(pkg = "Rpackages/rpsftmPlus")

# 6) copy site to correct location in repo for github sites hosting
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

# 3) reinstall the updated package
devtools::install(pkg = "Rpackages/MAIC")

# 4) update the file Rpackages/MAIC/_pkgdown.yml
# 5) rebuild documentation using pkgdown
pkgdown::build_site(pkg = "Rpackages/MAIC")

# 6) copy site to correct location in repo for github sites hosting
file.copy(from = "Rpackages/MAIC/docs", 
          to = "docs/Rpackages/MAIC", 
          overwrite = TRUE, 
          recursive = TRUE)


##############################################
# gemtcPlus
##############################################
# 1) make any updates to the package 
# 2) update documentation using devtools
devtools::document(pkg = "Rpackages/gemtcPlus")

# 3) reinstall the updated package
devtools::install(pkg = "Rpackages/gemtcPlus")

# 4) update the file Rpackages/gemtcPlus/_pkgdown.yml
# 5) rebuild documentation using pkgdown
pkgdown::build_site(pkg = "Rpackages/gemtcPlus")

# Note as the vignettes for gemtcPlus can take a long time to run it is
# also possible just to update partial sections by just running the 
# below functions without regenerating the vignettes
pkgdown::build_home(pkg = "Rpackages/gemtcPlus")
pkgdown::build_reference(pkg = "Rpackages/gemtcPlus")
pkgdown::build_articles_index(pkg = "Rpackages/gemtcPlus")

# 6) copy site to correct location in repo for github sites hosting
file.copy(from = "Rpackages/gemtcPlus/docs", 
          to = "docs/Rpackages/gemtcPlus", 
          overwrite = TRUE, 
          recursive = TRUE)