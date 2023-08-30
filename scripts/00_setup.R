
# Header library load script for all the replication files

options(stringsAsFactors = FALSE)
pkgs = c(
  "caret", "caretEnsemble", "data.table", "doParallel", "dplyr", "ggplot2", "ggthemes", "haven",
  "kableExtra", "knitr", "lmerTest", "lmtest", "plotrix", "quanteda", "quanteda.textstats",
  "stringi", "textdata", "textmatch", "textreg", "tidyverse", "tm", "xtable")

lapply(pkgs, require, character.only=T)
options(java.parameters = "-Xmx2g")
library(bartMachine)


#devtools::install_github("quanteda/quanteda.sentiment")
#devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.sentiment)
library(quanteda.dictionaries)


# Our library
devtools::install_github("reaganmozer/tada")
library(tada)



# Set up needed directories to save results
dir.create(here::here("results/"), showWarnings = FALSE )
dir.create(here::here("figures/"), showWarnings = FALSE )
dir.create(here::here("tables/"), showWarnings = FALSE )
dir.create(here::here("data-raw/"), showWarnings = FALSE )
dir.create(here::here("data-external/"), showWarnings = FALSE )
