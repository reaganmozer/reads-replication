
options(stringsAsFactors = FALSE, java.parameters = "-Xmx2g")

pkgs = c(
  "bartMachine", "caret", "caretEnsemble", "data.table", "devtools", "doParallel", "dplyr", "ggplot2", "ggthemes", "haven",
  "kableExtra", "knitr", "lmerTest", "lmtest", "plotrix", "quanteda", "quanteda.textstats", "rlang",
  "stringi", "textdata", "textmatch", "textreg", "tidyverse", "tm", "xtable")

lapply(pkgs, require, character.only=T)


#devtools::install_github("quanteda/quanteda.sentiment")
#devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.sentiment)
library(quanteda.dictionaries)


# Our library
#devtools::install_github("reaganmozer/rcttext")
library(rcttext)



# Set up needed directories to save results
dir.create(here::here("results/"), showWarnings = FALSE )
dir.create(here::here("figures/"), showWarnings = FALSE )
dir.create(here::here("tables/"), showWarnings = FALSE )
dir.create(here::here("data-raw/"), showWarnings = FALSE )
dir.create(here::here("data-external/"), showWarnings = FALSE )
