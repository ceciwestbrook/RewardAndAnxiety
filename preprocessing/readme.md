# Preprocessing directory
Ceci Westbrook, 2024

 # Analysis steps # 


 # Files in this directory and how to use them #
Data from participants in this study are de-identified but not anonymized. Therefore, for reasons of confidentiality, files including subject numbers, as well as behavioral data, are excluded from this repository, but available upon request from the author.

For analyses, in order of use:
convert_DICOMS.sh - this script takes the raw DICOM data and converts it into a BIDS-formatted dataset using dcm2niix.

fmriprep_cmd, _batch, _arraybatch etc. - the scripts used to run fmriprep. These are formatted different ways to run locally (cmd), or using SLURM batch and arraybatch functionality. The NONLONGITUDINAL version excludes the longitudinal option for subjects missing either pre- or posttreatment data.

make_motion_confound_files.sh - the next step is to make the confound files that will be needed for level1 FEATs. This script calls the R script (extract_motion_confounds.R) that reads in the fmriprep output, extracts and reformats the standard and extended motion parameters that will be used as confounds in FEAT. Note that this also puts out spike_numbers.txt which is an easy way to scan the number of censored TRs per subject and run, for QC purposes.

# Accessory files #
check_censored_trial_numbers.R - This script cross-references the censor file TRs against the TRs of the trial start times
to make sure we didn't censor out too many trials!

extract_motion_confounds.R - script to write out motion spike numbers for use in QA.
