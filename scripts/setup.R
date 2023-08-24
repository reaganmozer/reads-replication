
# Header library load script for all the replication files

options(stringsAsFactors = FALSE)

library(tm)
library(quanteda)
library(textmatch)
library(data.table)
library(tidyverse)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(quanteda.textstats)

#devtools::install_github( "https://github.com/cran/softmaxreg" )
library( softmaxreg )

# Our library
library( tada )



# Set up needed directories to save results
dir.create(here::here("Results/"), showWarnings = FALSE )
dir.create(here::here("Figures/"), showWarnings = FALSE )
dir.create(here::here("Tables/"), showWarnings = FALSE )
dir.create(here::here("data-raw/"), showWarnings = FALSE )
