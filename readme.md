# Reward Responses and Treatment Response to Psychotherapy In Adolescents With Anxiety Disorders
This is the accompanying analysis code for the above-named study which used an Approach-Avoid Conflict fMRI task in the context of the CATS study (Silk et al., 2018).
Please see our preregistration at https://osf.io/f2wzr for more information.

All code and associated documents were written by Ceci Westbrook 2021-2024 unless otherwise noted.

 # Task description: # 
Participants completed 2 runs with 8 randomized trials each of 4 conditions: approach reward (“approach”), avoid loss (“avoid”), ambiguous or conflict trials (“conflict”), and control trials. Participants were instructed prior to the start of the task that there was a “bank” on the right of the screen; that moving the approach (“$”) cue toward the bank would result in reward; moving the money-eating snake (“ö”) toward the bank would result in loss of points; moving the ambiguous (“?”) cue toward the bank would result in a reward most of the time; and they must move the control (“X”) cue but would not receive rewards. They were familiarized with the task and contingencies in a practice run outside of the scanner. Task regressors and timing are shown at the bottom of the figure; each color denotes a different regressor, which were further divided among task conditions. Of note, rewards for the task changed part-way through the study, such that the second half of participants earned actual monetary reward while the first half earned points exclusively. No differences were found between money- and no-money groups in behavioral outcomes, so they were both included in imaging analyses.

 # How to use this repository: # 
This repository does not include all files required to complete analyses! Files including PHI or participant numbers are not included in this public repository to preserve confidentiality. De-identified data are available from the authors upon request.

This dataset was analyzed primarily using FSL's Feat program, which takes design files (.fsf) as input. These files can be batch-edited and submitted, which was the approach used here, following Jeanette Mumford's approach (please see: https://www.youtube.com/playlist?list=PLB2iAtgpI4YHlH4sno3i3CUjCofI38a-3). Preprocessing was completed using fmriprep version 1.0.12 (https://fmriprep.org/en/stable/).
Code for this project was run on a parallel server using SLURM, and as such, scripts will contain syntax for batch processing using SLURM. These files can be run on local servers with minor editing to remove SLURM syntax, but this is up to the individual user.
The order of use for these scripts are:
 1) Preprocessing with fmriprep using scripts in the preprocessing directory
 2) level 1 analyses conducted by batch-editing the template .fsf scripts found in the level1/ directory and running the resulting files using feat
 2) adjusting the registration in level1 feat directories using Jeanette Mumford's workaround (https://mumfordbrainstats.tumblr.com/post/166054797696/feat-registration-workaround)
 4) level 2 analyses conducted by batch-editing template .fsfs in the level2/ directory and running the resulting files using feat, and finally
 5) group-level analyses using FEAT.

 # Data used in the study: # 
Structural and functional images were collected on a Siemens 3T Trio MRI scanner. 

Anatomical 
T1-weighted structural images (1 mm3 voxels) were acquired in the axial plane with an MPRAGE sequence (TR=2100ms, TE=3.31ms, flip angle = 8º, voxel size = 1mm3, matrix size 256x208, 176 slices)

Functional
Functional data were collected as T2*-weighted gradient-echo echo-planar images without parallel imaging, collected axially parallel to the anterior-posterior commissure line (TR = 1670ms, TE = 29ms, flip angle = 75º, voxel size = 3.0x3.0x3.0mm, matrix size 64x64, 32 interleaved slices). Two runs were collected of duration 5m24s (230 volumes). Participants made responses using an MRI-compatible button glove with their right hands.

Fieldmap
This was not collected on all participants and as such was excluded from the current analyses.

 # Preregistration: # 
This dataset corresponds to pre-registered analyses which can be found here: https://osf.io/f2wzr.

 # Associated neural data # 
This code is associated with neural data published by Sequeira et al., 2021. At the time of data collection, there were not large public repositories of neuroimaging data available, and participants did not consent to upload of their data to said repositories, so data are not publicly available. Participants consented to sharing of de-identified data upon request.

 # Data collection and acknowledgements: # 
Data were collected between 2009-2011 by the original CATS study (Trial registration: ClinicalTrials.gov NCT00774150). Supported by National Institute of Mental Health (NIMH) grant P50 MH080215. Support for research participant recruitment was also provided by the Clinical and Translational Science Institute at the University of Pittsburgh (NIH/NCRR/CTSA grant UL1 RR024153). The authors would like to thank Jeanette Mumford, PhD, for guidance on developing fMRI models.

 # Authors: # 
Cecilia A. Westbrook, Michael Schlund, Jennifer S. Silk, Erika E. Forbes, Neal D. Ryan, Ronald E Dahl, Dana McMakin, Philip C. Kendall, Anthony Mannarino, and Cecile D. Ladouceur.

 # Status of current dataset: # 
Data are currently under submission at the American Journal of Psychiatry. Publication information will be forthcoming as it becomes available.

