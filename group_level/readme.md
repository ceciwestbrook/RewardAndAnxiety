# group-level directory
Ceci Westbrook, 2024

 # Analysis steps # 
Group level analyses in FSL are conducted using higher-level analyses in FEAT. The scripts for those analyses are .fsfs contained in the results directory itself (not included in this repository). This directory includes files to help generate those scripts and also scripts for behavioral analyses.

 # Files in this directory and how to use them #
Data from participants in this study are de-identified but not anonymized. Therefore, for reasons of confidentiality, files including subject numbers, as well as behavioral data, are excluded from this repository, but available upon request from the author.

get_behav_vars.R - this is the main running script used to generate lists of directories and accompanying behavioral or categorical variables to paste into group-level FEAT analyses. This requires various accessory files including lists of subject numbers and behavioral data files which are not included in this repository. The only other thing needed to create the FEATs is the design matrix which can be obtained from the fsf file.
