README
======

This repository contains all data files and R-scripts used for the analyses
reported in the paper:

Montero-Melis, G., & Bylund, E. (accepted). Getting the ball rolling: the 
cross-linguistic conceptualization of caused motion. *Language and Cognition*.
doi:10.1017/langcog.2016.22

Please contact the first author ([GMM](mailto:montero-melis@biling.su.se)) if you are unable to reproduce the analyses or to report any other type of issue that needs to be fixed.


Files -- overview
-----------------

The repository contains data files, scripts to run the analyses and saved data.
The scripts and the data files should make it possible to reproduce all analyses,
especially if using the same package versions as reported in the original paper.
However, since some of the model fitting takes a long time, the fitted models
are also provided as saved data (in the form of R-objects, which have the file
extension ".RData").

### Data files

- `data_exp1_ling.csv` contains the by-subject data for linguistic descriptions
in Experiment 1; it shows, for each participant, the proportion of descriptions
that expressed each of the event components. This is a summary of
`data_norms.csv`
- `data_experiments_raw.csv` contains the raw data as obtained from the E-Prime
software in which the experiment was run. The E-prime output was heavily 
simplified, removing unnecessary columns and rows. This is the data that is
turned into the similarity data of `data_experiments_similarity.csv` with the
script `analysis_obtain-similarities.R`.
- `data_experiments_similarity.csv` is the similarity data on which the models
are fitted after minor processing. It is obtained from `data_experiments_raw.csv`
with the script `analysis_obtain-similarities.R`.
- `data_norms.csv` contains the data for the linguistic norming study (reported
under Experiment 1). Rows correspond to descriptions of target items by one
individual participants.
- `data_participants.csv` contains information about the participants in
Experiments 1-3 (but not from participants in the norming study).
- `data_videoclip-info.csv` contains info about the 32 target items.


### R analysis scripts

- `analysis_figures_results.R` reproduces the figures for all the reported analyses,
including the scatterplots and correlations in Exp 1 (cf. Fig. 3 and Table 2).
This script loads the fitted models, which can either be directly loaded from the
saved objects or reproduced running the script `analysis_fit_models.R`.
- `analysis_fit_models.R` fits all the models reported in the paper and saves 
them to file. Note that this might take a very long time---up to several days
on my Intel (R) Core(TM) i7 CPU L640, 4.00 GB RAM, running on Windows 7. For 
that reason, the models are also saved to disk as R-objects.
- `analysis_model_diagnostics.R` shows model summaries and some diagnostics
about collinearity of the predictors. This script also creates an enhanced
output from the model that shows coefficient estimates with significance codes
(these are reported in the Supplementary Materials).
- `analysis_norming_study.R` reproduces all analyses related to the norming study.
- `analysis_obtain-similarities.R` takes the raw data produced by the task in 
E-prime and converts the raw data (consisting of X- and Y-coordinates for each
placed item) into similarity data. Basically, it creates the data file
`data_experiments_similarity.csv`.


### R objects

The four R-objects correspond to each of the four fitted models reported in
the paper. Their names start with `fitted_models_fm`, the continuation 
indicates the model it contains.


Data files: description of variables
------------------------------------

### `data_exp1_ling.csv`

- *language*: L1 of the participant (Spanish or Swedish)
- *subject*: participant identifier
- *component*: Semantic component (i.e. semantic information)
- *Proportion*: Proportion of descriptions for that participant which included
information about that semantic component.


### `data_experiments_raw.csv` 

- *Subject*: participant identifier
- *Language*: L1 of the participant (Spanish or Swedish)
- *Encoding*: Under which encoding condition, i.e. which experiment (Exp 1 =
linguistic, Exp 2 = free, Exp 3 = interference)
- *Date*: date of data collection
- *Block*: each of the three blocks of which the task consisted
- *Trial*: Each of the 22 trials shown to a participant per block; each trial
consists of watching a scene and placing it on the screen (see Methods in article)
- *Scene*: Each of the 32 target scenes
- *View_time*: time ellapsed from the end of the scene until participants
clicked on a pop-up window to move to the arrangement screen (this variable
is not used in the analyses)
- *Categoriz_time*: time it took the participant to place the scene once they
had entered the arrangement screen.
- *X*: x-coordinate of the centre of the scene (i.e. thumbnail of the scene)
as placed by the participant.
- *Y*: same as *X* but the y-coordinate


### `data_experiments_similarity.csv` 

- *Subject*: participant identifier
- *Language*: L1 of the participant (Spanish or Swedish)
- *Encoding*: Under which encoding condition, i.e. which experiment (Exp 1 =
linguistic, Exp 2 = free, Exp 3 = interference)
- *Item*: Each of the 496 pairwise comparisons of the 32 target scenes 
arranged during the task.
- *P* (= Path): Binary variable to indicate whether the current Item (i.e. pair
of scenes) share the Path or not. If 1, the two scenes have the same Path
(e.g. up and up);
if 0, they have different paths (e.g. up and down)
- *MC* (= Manner of Cause): Same as *P* but for manner of cause (push/pull)
- *MO* (= Manner of Object): Same as *P* but for manner of object (roll/slide)
- *Di* (= Direction): Same as *P* but for direction (left-right/right-left)
- *Gr* (= Ground): Same as *P* but for ground (hill, cave, etc.)
- *Ob* (= Object): Same as *P* but for object (wheel, trunk, etc)
- *Sim*: Similarity score for that item (i.e. for that particular pair
of scenes)


### `data_norms.csv`

- *language*: L1 of the participant (Spanish or Swedish)
- *subject*: participant identifier
- *videoname*: Each of the 32 target scenes (same as "scene" in data sets above)
- *Path*, *MannerCause*, *MannerObject*, *Direction*, *Ground*, *Object*:
Binary variables indicating whether each of these semantic components were
expressed in the description (=1) or not (=0).
- *Verb_P*, *Verb_MC*, *Verb_MO*: Binary variables indicating whether the verb
encoded information about Path (P), Manner of cause (MC) or Manner of object 
(MO), respectively.
- *OutsideV_P*, *OutsideV_MC*, *OutsideV_MO*: Binary variables indicating whether
the description contained information about Path (P), Manner of cause (MC) or 
Manner of object (MO) outside of the verb.


### `data_participants.csv` 

This data table contains information about the participants, some of which
is not used in the analyses. I leave it there for reference and in case
someone would like to follow up the analyses (e.g. by looking into gender).

- *subject*: participant identifier
- *language*: L1 of the participant (Spanish or Swedish)
- *gender*: participant's gender (male or female)
- *age*: participant's age
- *date*: date (and sometimes time) of data collection
- *encoding*: Encoding condition (i.e. experiment in which participant took
part)
- *ling_order*: All participants described the events in one of four pseudo-
randomized orders; this variable indicates in which one. Note that participants
in Experiments 2 and 3 (free encoding and verbal interference) described the
scenes *after* the arrangement task and none of this linguistic data is reported
in this article.

### `data_videoclip-info.csv`

The variables describe each of the 32 target items with respect to their 
semantic components. It's the same table as provided in Supporting Materials
Table 1 (Description of target stimuli).
The last column shows the length of each scene in ms.
