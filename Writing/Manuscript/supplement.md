---
title: "Appendix for Are Donors Really Responding? Analyzing the Impact of Global Restrictions on NGOs"
short-title: Appendix for Are Donors Really Responding?
author:
- name: Suparna Chaudhry
  affiliation: Christopher Newport University
  email: suparna.chaudhry@cnu.edu
  url: http://www.suparnachaudhry.com/
- name: Andrew Heiss
  affiliation: Brigham Young University
  email: andrew_heiss@byu.edu
  url: https://www.andrewheiss.com/
date: October 28, 2018
title-page: false
published: 
git-repo: https://github.com/andrewheiss/donors-ngo-restrictions
reference-section-title: References
toc: true
toc-figures: false
toc-tables: false
appendix: true
---

\LTcapwidth=\textwidth

# Code

All figures and tables can be replicated using code and data available at `actual-url-here-later` [@r-project; @stan; @rstan; @brms-jss; @brms-rjournal; @ggplot2]. Additionally, a complete computing environment with a snapshot of R 3.5.1, Stan 2.17, and other packages can be installed through Docker at `actual-docker-url-here-later`.


# Data

## Missing data

The bulk of our new dataset has complete data for every variable in each country-year observation, with only a few variables from V-Dem and the World Bank suffering from missing data: Polity IV, the political corruption index, the civil society regulatory index, population, and GDP. These variables are not missing at random—most of the missing data can be attributable to a lack of consistent reporting. While multiple imputation is not inherently less biased than listwise deletion and thus not always necessary when data is not missing at random [@Pepinsky:2018], we impute our missing data in order to estimate consistent within and between effects in our models (see below for further explanation). We employ Bayesian multiple imputation using Amelia II [@HonakerKingBlackwell:2011]. We estimate individual regression models across five imputed datasets and then meld and combine the resulting posterior distributions. We varied the number of imputed datasets between 5 and 10 and found only trivial differences in coefficients, as predicted by @KingHonakerJoseph:2001. In the interest of computational efficiency and speed, we only use five imputed datasets in our final analysis.

Our imputation model predicts missing values using following variables: year, country ID, the civil society regulatory environment, corruption, GDP (logged), government effectiveness, natural disaster occurrence and severity, Polity IV, population (logged), trade as a percent of GDP, and total ODA. We also include lags and leads (future values) of the civil society regulatory environment, corruption, GDP, trade as a percent of GDP, Polity IV, and population.


## Summary statistics

\scriptsize
!INCLUDE "Output/tbl-var-summary.md"
\normalsize


## List of countries included

\scriptsize
!INCLUDE "Output/tbl-countries.md"
\normalsize

\clearpage


# Modeling

## Crossed random effects multilevel models

Following @BellJones:2015 we use crossed random effects for country and year and use a combination of meaned and demeaned versions of each continuous variable to estimate both the within and between effects of each variable. This approach has multiple benefits. The coefficients for the demeaned variables are roughly equivalent to their corresponding coefficients in a fixed effects model, but a fixed effects model assumes that the between effect (captured by the mean variables) is 0, which is not the case. A random effects model specified in this manner is more interpretable, as it clearly separates the within and between effects (within = demeaned, between = mean). Typical time-series-cross-sectional data analysis includes these variables as fixed effects to control out issues of heterogeneity within units. However, @BellJones:2015 forcefully (and convincingly) argue that fixed effects models eliminate too much variance and make it impossible to measure the effects of time-invariant (or slowly-variant) variables. Random effects (or multilevel) models, when properly specified, overcome these issues by decomposing the effects of variables to within- and between-effects (or time-variant and time-invariant effects). 

@tbl:within-between-example demonstrates the intuition behind this approach. Model 1 is a basic OLS model with country fixed effects. Model 2 is a basic OLS model with country random effects, but potentially misspecified, since the between and within effects are conflated. Model 3 is a basic OLS model with country random effects specified with between (mean; $\bar{x}_i$) and within (demeaned; $x_{it} - \bar{x}_i$) coefficients. The demeaned/within coefficients in Model 3 are identical to the fixed effects coefficients in Model 1. If rows had been dropped because of listwise deletion (e.g., if there were missing values in one of independent variables), the coefficients would be slightly off, since the demeaned values would have been based on group means that included the values that were dropped (e.g. all 2013 rows are dropped because of lags, but the group means included 2013). We use multiple imputation to avoid this issue—we need the data to be as complete as possible to get the most accurate random effects.

\footnotesize

!INCLUDE "Output/tbl-within-between-example.md"

Table: Example of crossed random effects multilevel modeling; dependent variable is log of ODA in previous year {#tbl:within-between-example}

\normalsize

## Prior distributions

We use weakly informative prior distributions for each of the coefficient parameters, based on a normal distribution with a mean of zero. We obtain the posterior distribution of each dependent variable with Markov Chain Monte Carlo (MCMC) sampling and simulate values from the joint posterior distribution of the coefficient parameters. We use Stan through R to generate 4 MCMC chains with 2,000 iterations in each chain, 1,000 of which are used for warmup. All chains converge; we assess convergence with visual inspection. We use the medians of the simulated values from the MCMC samples as coefficient estimates and use the 5% and 95% quantiles as lower and upper limits for 90% credible intervals. Following the suggestion of @GelmanCarlin:2014, we rely on 90% credible intervals for computational stability and for better estimation of Type-S errors. Finally, we estimate models on each of the imputed datasets individually and merge the resulting MCMC chains and posterior distributions.

## Full model results

\scriptsize
!INCLUDE "Output/tbl-h1-coefs-bayes.md"
\normalsize

\clearpage

\scriptsize
!INCLUDE "Output/tbl-h2-coefs-bayes.md"
\normalsize

\clearpage

\scriptsize
!INCLUDE "Output/tbl-h3-domestic-coefs-bayes.md"
\normalsize

\clearpage

\scriptsize
!INCLUDE "Output/tbl-h3-foreign-coefs-bayes.md"
\normalsize

\clearpage


## Zero-one-inflated beta regression

To avoid making logit transformations of our proportion outcomes in H~2~ and H~3~, we run zero-one-inflated beta regression models as an additional robustness check. While the functional form of this model fits our data better (i.e. there are many country-year observations that received either no contentious aid or 100% contentious aid), we cannot make a perfect one-to-one translation of the coefficients in our primary logit-transformed models and these zero-one-inflated models. These models are run in multiple steps. The first step models the presence or absence of the dependent variable at 0%, followed by a model that explains the present or absence of a 100% outcome, followed by a model that explains variation for the range of outcomes between 1–99%. The coefficients apply to the final stage of the model and only describe the effects of our explanatory variables on the level of contentious or NGO-channeled aid, not whether or not it exists at 0% or 100%. As such, the effects of these coefficients are dampened from what we see in the main paper. However, the coefficients tend to move in the same direction in each model, showing that the effects are similar across functional forms.

\scriptsize
!INCLUDE "Output/tbl-h2-coefs-bayes-zoib.md"
\normalsize

\scriptsize
!INCLUDE "Output/tbl-h3-domestic-coefs-bayes-zoib.md"
\normalsize

\scriptsize
!INCLUDE "Output/tbl-h3-foreign-coefs-bayes-zoib.md"
\normalsize
