mod.h1.advocacy.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_between + advocacy_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_within + corruption_between +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h1.entry.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    entry_between + entry_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_within + corruption_between +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h1.funding.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    funding_between + funding_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_within + corruption_between +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
