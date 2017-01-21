mod.h1.advocacy.total <- function(df, outcome) {
  indep.vars <- ~ 
    advocacy_between + advocacy_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    total.oda_log_between + total.oda_log_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.entry.total <- function(df, outcome) {
  indep.vars <- ~ 
    entry_between + entry_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    total.oda_log_between + total.oda_log_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}

mod.h1.funding.total <- function(df, outcome) {
  indep.vars <- ~ 
    funding_between + funding_within +
    polity_between + polity_within + 
    gdp.capita_log_between + gdp.capita_log_within +
    trade.pct.gdp_between + trade.pct.gdp_within + 
    corruption_between + corruption_within +
    total.oda_log_between + total.oda_log_within +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data=df)
}
