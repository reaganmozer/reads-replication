## About 

This branch of this repository is a simplified version of the replication files for **"Combining human and automated scoring methods in experimental assessments of writing: a case study tutorial"** by Mozer et al. (2023), forthcoming in the *Journal of Educational and Behavioral Statistics* [(publisher link)](https://journals.sagepub.com/doi/full/10.3102/10769986231207886).

The goal of this branch is to make an easily usable vignette that shows how to use the `rcttext` package.



## Replication Data
The data used in this study has been made publicly available through the Harvard Dataverse and can be accessed [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/J9KSHU). Click the "Access Dataset" button and complete the data use agreement to download the relevant files.


## System Requirements
Replication scripts require the following packages to be installed from Github:

```{r}
## rcttext and textmatch development packages
devtools::install_github("reaganmozer/rcttext")
devtools::install_github("reaganmozer/textmatch")

## quanteda add-ons
devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("kbenoit/quanteda.dictionaries")
```	

## Instructions
Once the initial raw data have been downloaded, put both CSV files into the `data-raw` folder of the directory, then run the scripts in the `scripts` folder in the following order:

1. Run `00_setup.R` for initial configurations.
2. Execute data processing scripts (`01_process_pilot.R`, `02_process_text.R`, `03_make_text_features.R`) in the order listed.
3. Run impact estimation scripts (`04_estimate_impacts.R`, `05_compare_concept_words.R`).
4. Proceed with analysis scripts (`06_run_ccs.R`, `07_train_ML_pilot.R`, `08_estimate_ML_impacts.R`).
5. Finally, execute the results presentation scripts (`09_make_tables.R`, `10_make_figures.R`).

Notes: scripts 1-8 generate intermediate data files that are stored in the `data-generated` folder as well as results and figures that are stored in the appropriate folders.
The `data-external` folder is used to hold files generated for processing in external programs (e.g., LIWC and TAACO).

## Details

The `scripts` folder contains the R code needed to replicate the analysis in Mozer et al. (2023). 

#### Setup \& Data Processing
- `00_setup.R`: Initial setup configurations for the project.
- `01_process_pilot.R`: Processes pilot data for training the machine learning model.
- `02_process_text.R`: Processes and cleans essay texts.
- `03_make_text_features.R`: Generates a large set of features for the text corpus.

#### Impact Estimation
- `04_estimate_impacts.R`: Estimates impacts on an array of text outcomes.
- `05_compare_concept_words.R`: Estimates impacts on the usage of pre-identified concept words.

#### Analysis
- `06_run_ccs.R`: Performs a Concise Comparative Summarization (CCS) analysis on the essays.
- `07_train_ML_pilot.R`: Trains a machine learning model for predicting human-coded quality scores based on pilot data.
- `08_estimate_ML_impacts.R`: Estimates treatment effects on fully machine-generated proxy outcomes.

#### Results Presentation
- `09_make_tables.R`: Generates tables for summarizing data or results.
- `10_make_figures.R`: Creates figures for data visualization.

#### General Purpose Scripts
- `utils.R`: Utility functions for text cleaning and data manipulation.
- `cluster_threshold_C.R`: Determines the penalty `C` for a text regression model with clustering at the school level



# Note on data-external
This folder holds files generated for external processing (e.g., LIWC).
They are not saved on git as they will have the raw data.

# Note on data
There are a few small files here that have, for example, the reference texts students were given before they were asked to write their essays.

