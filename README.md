## Plotting scripts for "Metabolomic and genomic prediction of common diseases in 700,217 participants in three national biobanks"

### Description

This repository contains scripts related to paper "Metabolomic and genomic prediction of common diseases in 700,217 participants in three national biobanks". We provide scripts for reproducting the figures in the paper from the Source Data provided.

### Requirements

The code has been tested with R version 4.3.2. It requires the following packages:

- tidyverse
- dplyr
- readr
- scales
- ggplot2
- tidyr
- ggforestplot
- ggh4x
- cowplot
- purrr
- survminer
- grid
- gridExtra
- egg

### Content
#### Source data

Source data, in csv format, is located under `source-data`. These files include:

* Figure1a_plotdata.csv - the estimated 4-year incidence rate of each disease for individuals within each percentile of metabolomic score for that disease, averaged across the 3 biobanks
* Figure1b_plotdata.csv - the 4-year hazard ratios for developing incident disease for individuals in the top 10% of each metabolomic score within the three biobanks and the meta-analysis results
* Figure2a_plotdata.csv - the 10-year hazard ratios for developing incident disease for individuals in the top 10% of each metabolomic, genomic and combined score in the UK Biobank
* Figure2b_plotdata.csv - cumulative disease-free survival rates after baseline for high-polygenic/low-metabolomic risk, high-polygenic/high-metabolomic risk and low-polygenic risk groups for each disease
* Figure2c_stratified_plotdata.csv - time-stratified hazard ratios for each 1-year interval after baseline for individuals in the top 10% of each metabolomic and genomic score for each disease
* Figure2c_tvc_plotdata.csv - time-varying estimates of hazard ratios using restricted cubic splines for individuals in the top 10% of each metabolomic and genomic score for each disease
* Figure3_plotdata.csv - cumulative disease-free survival rates after follow-up visitor for individuals with repeat samples who stayed in, left, joined or never entered the top 10% metabolomic score for each disease
* Figure4a_plotdata.csv - the 10-year hazard ratios for developing incident disease for individuals in the top 10% of each clinic, multiomoc and combined score in the UK Biobank
* Figure4b_plotdata.csv - calibration data (predicted and actual incident disease rates for each score decile) for metabolomic scores for each disease in the three cohorts

#### Figures

Scripts for making the figures presented in the manuscript, based on the source data, are located under `figure-creation`. These scripts include:

* Figure1__percentile_plots_and_hazard_ratios_by_decile.R
* Figure2__performance_genetics_versus_metabolomics.R
* Figure3__modifiability_of_risk_factors.R
* Figure4__clinical_scores_and_calibration_curves.R
