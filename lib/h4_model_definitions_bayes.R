mod.h4.barriers.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h4.barriers.new.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_new +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h4.type.total.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy +
    entry +
    funding +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h4.type.new.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_new + entry_new + funding_new +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h4.csre.bayes <- function(df, outcome) {
  indep.vars <- ~ 
    csre +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  stan_glmer(build.formula(outcome, indep.vars),
             data = df, family = gaussian(),
             prior = normal(), prior_intercept = normal(),
             chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
