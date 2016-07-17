README
======

This repository contains all data files and R-scripts used for the analyses
reported in the paper:

Montero-Melis, G., & Bylund, E. (accepted). Getting the ball rolling: the cross-linguistic conceptualization of caused motion. Language and Cognition. doi:10.1017/langcog.2016.22

Please contact the first author (GMM) if you are unable to reproduce the analyses or to report any other type of issue that needs to be fixed.


Files
-----

The repository contains data files, scripts to run the analyses and saved data.
The scripts and the data files should make it possible to reproduce all analyses,
especially if using the same package versions as reported in the original paper.
However, since some of the model fitting takes a long time, the fitted models
are also provided as saved data (in the form of R-objects, which have the file
extension ".RData").

### Data files

- `data_exp1_ling.csv` contains the by-subject data for linguistic descriptions in Experiment 1; this shows for each participant the proportion of descriptions that expressed each of the event components.
- `data_norms.csv` contains the data for the linguistic norming study (reported under Experiment 1).
- `data_participants.csv` contains information about the participants in Experiments 1-3 (but not from participants in the norming study)


### R scripts

- `analysis_figures_results.R` reproduces the figures for all the reported analyses, including the scatterplots and correlations in Exp 1 (cf. Fig. 3 and Table 2). This script loads the fitted models, which can either be directly loaded from the saved objects or reproduced running the script `analysis_fit_models.R`.
- `analysis_fit_models.R` fits all the models reported in the paper and saves them to file. Note that this might take a very long time---up to several days on my Intel (R) Core(TM) i7 CPU L640, 4.00 GB RAM, running on Windows 7.
- `analysis_norming_study.R` reproduces all analyses related to the norming study.


Abbreviations
-------------

The following abbreviations for semantic components are sometimes used, for
example in variable names:

- P = Path
- MC = Manner of Cause
- MO = Manner of object
- Di = Direction (left-right/right-left)
- Gr = Ground
- Ob = Object
