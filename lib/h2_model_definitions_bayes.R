ivs.h2.barriers.total <- ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious_logit +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

ivs.h2.type.total <- ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious_logit +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

ivs.h2.csre <- ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious_logit +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

h2.ivs <- c(get.terms(ivs.h2.barriers.total), 
            get.terms(ivs.h2.type.total), 
            get.terms(ivs.h2.csre)) %>% 
  unique()

mod.h2.barriers.total.bayes <- function(df, outcome) { 
  cat("#####\n")
  cat("Running model for mod.h2.barriers.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h2.barriers.total),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.type.total.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h2.type.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h2.type.total),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.csre.bayes <- function(df, outcome) {  
  cat("#####\n")
  cat("Running model for mod.h2.csre.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h2.csre),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
