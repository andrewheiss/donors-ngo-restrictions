mod.h1.neutral.reg <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_register +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    total.oda_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h1.neutral.fund <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_disclose_funds +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    total.oda_log +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h2.neutral.reg <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_register +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h2.neutral.fund <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_disclose_funds +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.contentious_logit +
    internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.dom.neutral.reg <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_register +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.dom_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.dom.neutral.fund <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_disclose_funds +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.dom_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.for.neutral.reg <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_register +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.foreign_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}

mod.h3.for.neutral.fund <- function(df, outcome) {
  indep.vars <- ~ 
    ngo_disclose_funds +
    polity_within + polity_between +
    gdp.capita_log_within + gdp.capita_log_between +
    trade.pct.gdp_within + trade.pct.gdp_between +
    corruption_within + corruption_between +
    prop.ngo.foreign_logit +
    internal.conflict.past.5 + natural_disaster.occurrence +
    (1 | cowcode) + (1 | year)
  
  lmer(build.formula(outcome, indep.vars), data = df)
}
