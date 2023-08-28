
# Header library load script for all the replication files

options(stringsAsFactors = FALSE)

library(tm)
library(quanteda)
library(textmatch)
library(data.table)
library(tidyverse)
library(quanteda.textstats)
library(textdata)

#devtools::install_github("quanteda/quanteda.sentiment")
#devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.sentiment)
library(quanteda.dictionaries)


# Our library
library( tada )



# Set up needed directories to save results
dir.create(here::here("Results/"), showWarnings = FALSE )
dir.create(here::here("Figures/"), showWarnings = FALSE )
dir.create(here::here("Tables/"), showWarnings = FALSE )
dir.create(here::here("data-raw/"), showWarnings = FALSE )
