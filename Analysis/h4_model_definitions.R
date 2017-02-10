mod.h4.barriers.total <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h4.barriers.new <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_new +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h4.type.total <- function(df, outcome) {
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
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h4.type.new <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_new + entry_new + funding_new +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h4.csre <- function(df, outcome) {
  indep.vars <- ~ 
    csre +
    polity +
    gdp.capita_log +
    trade.pct.gdp +
    corruption +
    nb_oda.sum_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}
