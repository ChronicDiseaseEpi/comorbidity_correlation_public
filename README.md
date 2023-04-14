# comorbidity_correlation_public
Code, aggregate level data and outputs for "Correlations between comorbidities in trials and the community: an individual-level participant data meta-analysis"

If you have any issues with the code or dependent files please raise an issue and 
we will do our best to rectify the problem as soon as possible. 

We do not directly provide the model fits for the trials or community (too big) but instead
provide data frames of the cleaned final model outputs. 

Model outputs:
- Omega summaries: Correlation estimates summarised across all MCMC draws (presented in paper)
- Omega draws: MCMC draws from each model with correlation estimate from each MCMC iteration


# To recreate the analysis run the scripts in order

The "Data" folders contains the data needed to simulate the community pseudo IPD (all exported from SAIL):

- "SAIL_aggregate_data.html"         - Counts of number of individuals with each comorbidity combination   
- "SAIL_no_comorbidities_strata.csv" - Counts of number of individuals with no comorbidities
- "dementia_no_schz_rand_sd.csv"     - Replacement aggregate level data for dementia
- "community_aggregate_data.rds"     - List of aggregate dataframes

The community analysis was split into 2 parts: 
- 11 models ran (1 model per index condition) on an external virtual machine (vm)
- 5 models ran in SAIL

The 11 models ran on the vm are the model scripts 02-12 (non age restricted) and 15-25 (age restricted). 

The models ran on SAIL have not been provided. However scripts 33 and 34 simulate the MCMC draws from the models that were ran on SAIL.

Trial analysis was done across 3 locations (See supporting folder):
- Vivli safe haven 
- GSK external
- Yoda safe haven 

To protect patient confidentiality we are not able to provide the trial data
which the models were fitted to. However, we do provide all the code we used to 
extract the trial data, identify comorbidities and run the MVP model in the "Supporting"
folder. The final cleaned trial model outputs are: 

  - GSK_omega_draws
  - GSK_trial_omega_summaries
  - yoda_omega_draws_final
  - yoda_omega_summary_final
  - vivli_omega_draws
  - vivli_omega_summary_final



Please note: The senior authorship of the publication that this repository accompanies was randomly assigned using the following code:

senior_auths <- c('Fergus','David')
set.seed(18591208)
senior_auths[rbinom(1, 1, 0.5)+1]

Result = David (28/02/2023)
