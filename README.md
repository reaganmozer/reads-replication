# Replication materials for  "Combining human and automated scoring methods in experimental assessments of writing: a case study tutorial"
================


This repository contains the materials needed to replicate the analyses presented in **"Combining human and automated scoring methods in experimental assessments of writing: a case study tutorial"** by Mozer et al. (2023), forthcoming in the *Journal of Educational and Behavioral Statistics*

## Instructions
First download the initial raw data and put it in the `data-raw` directory.
Once the initial data are downloaded,  run the scripts in the `scripts` folder in order.
These scripts will generate intermediate data or results, and store them in appropriate folders.

### Notes on generated intermediate data files
These scripts generate intermediate data files that are stored in `data-generated`.
Many (not all) of these generated files are in the git repo.
Some of the intermediate files have the original raw data, and are thus not committed.  The scripts will regenerate them and save them in the folder, if you have the original raw data.

### Note on data-external
This folder is to hold files generated for external processing (e.g., LIWC and TAACO).
