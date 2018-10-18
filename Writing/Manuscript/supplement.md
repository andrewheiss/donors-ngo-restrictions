---
title: "Supplement to Are Donors Really Responding? Analyzing the Impact of Global Restrictions on NGOs"
short-title: Supplement to Are Donors Really Responding?
author:
- name: Suparna Chaudhry
  affiliation: Christopher Newport University
  email: suparna.chaudhry@cnu.edu
  url: http://www.suparnachaudhry.com/
- name: Andrew Heiss
  affiliation: Brigham Young University
  email: andrew_heiss@byu.edu
  url: https://www.andrewheiss.com/
date: October 17, 2018
title-page: false
published: 
git-repo: https://github.com/andrewheiss/donors-ngo-restrictions
reference-section-title: References
toc: true
appendix: true
---

## Summary statistics

\small
!INCLUDE "Output/tbl-var-summary.md"
\normalsize

\clearpage

## List of countries included

\small
!INCLUDE "Output/tbl-countries.md"
\normalsize

\clearpage

## Crossed random effects multilevel modeling with time-series cross-sectional data

Following @BellJones:2015 we use crossed random effects for country and year and use a combination of meaned and demeaned versions of each continuous variable to estimate both the within and between effects of each variable. This approach has multiple benefits. The coefficients for the demeaned variables are roughly equivalent to their corresponding coefficients in a fixed effects model, but a fixed effects model assumes that the between effect (captured by the mean variables) is 0, which is not the case. A random effects model specified in this manner is more interpretable, as it clearly separates the within and between effects (within = demeaned, between = mean). Typical time-series-cross-sectional data analysis includes these variables as fixed effects to control out issues of heterogeneity within units. However, @BellJones:2015 forcefully (and convincingly) argue that fixed effects models eliminate too much variance and make it impossible to measure the effects of time-invariant (or slowly-variant) variables. Random effects (or multilevel) models, when properly specified, overcome these issues by decomposing the effects of variables to within- and between-effects (or time-variant and time-invariant effects). 

@tbl:within-between-example demonstrates the intuition behind this approach. Model 1 is a basic OLS model with country fixed effects. Model 2 is a basic OLS model with country random effects, but potentially misspecified, since the between and within effects are conflated. Model 3 is a basic OLS model with country random effects specified with between (mean; $\bar{x}_i$) and within (demeaned; $x_{it} - \bar{x}_i$) coefficients. The demeaned/within coefficients in Model 3 are identical to the fixed effects coefficients in Model 1. If rows had been dropped because of listwise deletion (like, if there were missing values in one of independent variables), the coefficients would be slightly off, since the demeaned values would have been based on group means that included the values that were dropped (e.g. all 2013 rows are dropped because of lags, but the group means included 2013). This is not a problem in these example models, but in our actual models we use multiple imputation to avoid this issue—we need the data to be as complete as possible to get the most accurate random effects.

\newpage

!INCLUDE "Output/tbl-within-between-example.md"

Table: Example of crossed random effects multilevel modeling; dependent variable is log of ODA in previous year {#tbl:within-between-example}

\clearpage

## Full model results

!INCLUDE "Output/tbl-h1-coefs-bayes.md"

\clearpage

!INCLUDE "Output/tbl-h2-coefs-bayes.md"

\clearpage

!INCLUDE "Output/tbl-h3-domestic-coefs-bayes.md"

\clearpage

!INCLUDE "Output/tbl-h3-foreign-coefs-bayes.md"
