mod.h2.barriers.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_within + barriers.total_between +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h2.barriers.new.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_new +
    polity_within + polity_between + 
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h2.type.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
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
  
  stan_glmer(build.formula(outcome, indep.vars),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h2.type.new.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_new + entry_new + funding_new +
    polity_within + polity_between + 
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}

mod.h2.csre.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    csre_within + csre_between +
    polity_within + polity_between + 
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data=df, family=gaussian(),
             prior=normal(), prior_intercept=normal(),
             chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
}
