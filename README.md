# The READS analysis replication file

This replicates the READS analysis from the paper.
It also demos the `tada` package that automates a lot of the feature generation.

# Using these scripts
First download the initial raw data and put it in the `data-raw` directory.
You can also download the word embedding file (glove), and put it in `data-raw` as well.
This folder is for data that everything is built on, that we do not have in this git repository.
We (will eventually) provide scripts to automate these initial steps.

Once the initial data are downloaded, you should be able to run the scripts in the `scripts` folder in order.
These scripts will generate intermediate data or results, and store them in appropriate folders.
You can actually run the scripts out of order, if the files needed by the script are present; the top of the script will load those files so it should be easy to tell if you are ready to run any given script.

# Notes on generated intermediate data files
These scripts generate intermediate data files that are stored in `data-generated`.
Many (not all) of these generated files are in the git repo.
Some of the intermediate files have the original raw data, and are thus not commited.  The scripts will regenerate them and save them in the folder, if you have the original raw data.

# note on data-external
This folder is to hold files generated for external processing (e.g., LIWC).
They are not saved on git as they will have the raw data.
