library("devtools")
library("roxygen2")
library("dplyr")

#getwd()
#create("rpsftmPlus")
#use_vignette("rpsftmPlus-vignette")

build_vignettes()

# copy to inst so do not need to rebuild with install and can link to git version
inst_dir <- "inst"
if (!dir.exists(inst_dir)){
  dir.create(inst_dir)
}

inst_doc_dir <- "inst/doc"
if (!dir.exists(inst_doc_dir)){
  dir.create(inst_doc_dir)
}

file.copy(from = "doc", to = "inst", recursive = TRUE)

document()


