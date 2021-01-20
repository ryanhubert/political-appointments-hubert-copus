# Political Appointments and Outcomes in Federal District Courts

This repository contains replication materials for "Political Appointments and
Outcomes in Federal District Courts" by Ryan Hübert and Ryan Copus, which is
forthcoming in the _Journal of Politics_. 

Download the manuscript and online appendix [here](https://ryanhubert.com/publications/revisiting-ideology/).  

## Software Requirements

The analysis for this article was conducted on a macOS machine (version 11.1, 
Big Sur) in both `python` (version 3.7.3) and `R` (version 4.0.3). The file
`requirements.txt` indicates which `python` modules you must install
to replicate the analysis. You must also have several libraries available in 
`R`, including `tidyverse`, `estimatr` and several libraries used to make plots 
(as well as any required dependencies).

Note that the `h2o` package used in the analysis may require additional
installation steps, see:
[https://docs.h2o.ai/h2o/latest-stable/h2o-docs/downloading.html#install-in-python](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/downloading.html#install-in-python).

## Data

Please refer to the article and the online appendix for detailed
information about the data collection and cleaning.

In the article, we analyze two datasets:

- `USDC-DATASET-JOP.csv` contains civil rights cases filed in seven U.S.
  District Courts between 1995 and 2016. (Note: for one district we only have
  data from 1996 to 2015.)

- `USDC-APPEALS-JOP.csv` contains all civil rights appeals filed in the U.S.
  Court of Appeals for the Ninth Circuit between 1996 and 2012.

We provide codebooks for the variables in each dataset: see
`codebook-main.txt` and `codebook-appeals.txt`.

The archived version of the original dataset compiled for use on this project is
available at [xxx], as well as in this GitHub repository.

## Replication Procedure

To replicate the analysis, you should start by creating a working directory on
your local machine, downloading the replication files from this GitHub
repository (or from the copy available in the Harvard Dataverse: [xxx]) and
moving the replication files into your working directory.

Then, you should open each `python` and `R` script and insert the full path to
your working directory as a character string that defines the variable `root`.

Then, you can execute the following scripts in this order:

1. `01_Preprocess.R`: This `R` script does some minimal cleaning the dataset
   consistent with the description in the article and online appendix.
   It also defines a function that implements a procedure used to run the
   regression model described in the article (see equation (1) in the main
   text).

2. `02_Predictions.py`: This `python` script implements the machine learning
   balance test described in the article for both our main analysis of the
   district courts, as well as our auxiliary analysis of the Ninth Circuit.

3. `03_Analysis_Appeals.R`: This `python` script conducts the auxiliary analysis
   on the Ninth Circuit dataset that is discussed in article.

4. `04_Analysis_Main.R`: This `R` script conducts the statistical analyses from the article and
   the online appendix, and generates the corresponding figures in
   the paper and online appendix.

5. `05_Outputs.R`: This `R` script imports the results generated from the
   previous scripts and generates tables and figures for the article and
   online appendix.

Each script contains detailed comments describing the various steps in the
analysis and should be straight-forward to execute.

## Comments and Questions

If you have any comments or questions about these replication files, please
contact Ryan Hübert using the contact information on his website:
[https://ryanhubert.com/](https://ryanhubert.com/).
