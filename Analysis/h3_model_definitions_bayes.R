ivs.h3.barriers.total <- ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.barriers.new <- ~ 
  barriers.total_new +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.type.total <- ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.type.new <- ~ 
  advocacy_new + entry_new + funding_new +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.csre <- ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

h3.ivs <- c(get.terms(ivs.h3.barriers.total), 
            get.terms(ivs.h3.barriers.new), 
            get.terms(ivs.h3.type.total), 
            get.terms(ivs.h3.type.new), 
            get.terms(ivs.h3.csre)) %>% 
  unique()

mod.h3.barriers.total.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.barriers.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.barriers.total),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.barriers.new.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.barriers.new.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.barriers.new),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.type.total.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.type.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.type.total),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.type.new.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.type.new.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.type.new),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.csre.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.csre.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.csre),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

# Foreign proportion models
ivs.h3.foreign.barriers.total <- ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.foreign.barriers.new <- ~ 
  barriers.total_new +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.foreign.type.total <- ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.foreign.type.new <- ~ 
  advocacy_new + entry_new + funding_new +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

ivs.h3.foreign.csre <- ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

h3.foreign.ivs <- c(get.terms(ivs.h3.foreign.barriers.total), 
                    get.terms(ivs.h3.foreign.barriers.new), 
                    get.terms(ivs.h3.foreign.type.total), 
                    get.terms(ivs.h3.foreign.type.new), 
                    get.terms(ivs.h3.foreign.csre)) %>% 
  unique()

mod.h3.foreign.barriers.total.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.barriers.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.foreign.barriers.total),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.foreign.barriers.new.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.barriers.new.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.foreign.barriers.new),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.foreign.type.total.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.type.total.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.foreign.type.total),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.foreign.type.new.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.type.new.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.foreign.type.new),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h3.foreign.csre.bayes <- function(df, outcome) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.csre.bayes \n")
  cat("#####\n")
  
  stan_glmer(build.formula(outcome, ivs.h3.foreign.csre),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}
