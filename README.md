# Replication materials for Mozer et al. (2023) 

This repository contains the materials needed to replicate the analyses presented in the article "Combining human and automated scoring methods in experimental assessments of writing: a case study tutorial" by Mozer et al. (2023), forthcoming in the *Journal of Educational and Behavioral Statistics"

# Using these scripts
First download the initial raw data and put it in the `data-raw` directory.
Once the initial data are downloaded, you should be able to run the scripts in the `scripts` folder in order.
These scripts will generate intermediate data or results, and store them in appropriate folders.
You can actually run the scripts out of order, if the files needed by the script are present; the top of the script will load those files so it should be easy to tell if you are ready to run any given script.

# Notes on generated intermediate data files
These scripts generate intermediate data files that are stored in `data-generated`.
Many (not all) of these generated files are in the git repo.
Some of the intermediate files have the original raw data, and are thus not committed.  The scripts will regenerate them and save them in the folder, if you have the original raw data.

# note on data-external
This folder is to hold files generated for external processing (e.g., LIWC).
They are not saved on git as they will have the raw data.
