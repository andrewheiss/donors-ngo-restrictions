mod.h3.barriers.total <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_within + barriers.total_between +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.dom_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.type.total <- function(df, outcome) {
  indep.vars <- ~ 
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
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.csre <- function(df, outcome) {
  indep.vars <- ~ 
    csre_within + csre_between +
    polity_within + polity_between + 
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.dom_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

# Foreign proportion models
mod.h3.foreign.barriers.total <- function(df, outcome) {
  indep.vars <- ~ 
    barriers.total_within + barriers.total_between +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.foreign_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.foreign.type.total <- function(df, outcome) {
  indep.vars <- ~ 
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
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.foreign.csre <- function(df, outcome) {
  indep.vars <- ~ 
    csre_within + csre_between +
    polity_within + polity_between + 
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.foreign_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}
