# script to update the documentation and vignettes hosted at https://roche.github.io/Global-HTA-Evidence-Open/index.html
# currently just redirects to the seperate repositories following refactor

# update the index page (just converts the README.md to html)
markdown::markdownToHTML("README.md", output = "docs/index.html")

# add redirects for the moved package documentation
markdown::markdownToHTML("Rpackages/flexsurvPlus/README.md", output = "docs/Rpackages/flexsurvPlus/docs/index.html")
markdown::markdownToHTML("Rpackages/gemtcPlus/README.md", output = "docs/Rpackages/gemtcPlus/docs/index.html")
markdown::markdownToHTML("Rpackages/rpsftmPlus/README.md", output = "docs/Rpackages/rpsftmPlus/docs/index.html")
markdown::markdownToHTML("Rpackages/MAIC/README.md", output = "docs/Rpackages/MAIC/docs/index.html")
markdown::markdownToHTML("Rpackages/descem/README.md", output = "docs/Rpackages/descem/docs/index.html")
