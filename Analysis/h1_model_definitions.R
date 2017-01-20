mod.h1.barriers.total <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_between + barriers.total_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.barriers.new <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_new +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.type.total <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_between + advocacy_within +
    entry_between + entry_within +
    funding_between + funding_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.type.new <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_new + entry_new + funding_new +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.csre <- function(df, outcome) {
  indep.vars <- ~ 
    csre_between + csre_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}
