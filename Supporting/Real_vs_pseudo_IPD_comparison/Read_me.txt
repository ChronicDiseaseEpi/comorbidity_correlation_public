This folder contains model outputs from Bayesian multivariate probit models fit using the Rstan package.

The models were fitted to examine correlations between different comorbidities across different "index conditions". We fitted models for 13 "index conditions". The model outcomes were the 6 commonest comorbidites for that index condition (these differed by condition). The model covariates were age and sex (the coefficients for these are not included in the current output, but we may need to ask for these in future if asked to provide these by reviewers). We fitted the models twice. Once using real IPD and once using pseudo-IPD created by aggregating the data then taking a random sample from the aggregated data.

We did these analyses in order to test whether correlations estimated from the aggregated data were similar to correlations estimated using real data. Each row in the csv file is a unique combination of index condition, model terms and whether the model was fit to real data or pseudo-IPD.

The model estimates were converted into scientific notation with 3 significant figures. The model diagnostics (n_eff and Rhat) were rounded to 1 and 2 DP respectively.

index_condition - the index condition eg, asthma, COPD
n - people with condition
term1 - a comorbidity
term2 - a different comorbidity
data_type - real or pseudo IPD
mean - the point estimate for the correlation between term1 and term2 comorbidities
se_mean - the Monte Carlo standard error
sd - the standard deviation of the posterior for the correlation
2.5%  - the 2.5th  centile of the posterior for the correlation
25%   - the 25th   centile of the posterior for the correlation
50%   - the 50th   centile of the posterior for the correlation
75%   - the 50th   centile of the posterior for the correlation
97.5% - the 97.5th centile of the posterior for the correlation
n_eff - a model diagnostic - the effective sample size in the MCMC
Rhat - a model diagnostic for convergence
