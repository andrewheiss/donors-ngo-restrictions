mod.h1.barriers.total <- function(df) {
  lmer(
    total.oda_log_next_year ~ 
      barriers.total_between + barriers.total_within +
      polity_between + polity_within + 
      gdp.capita_log_between + gdp.capita_log_within +
      trade.pct.gdp_between + trade.pct.gdp_within + 
      corruption_between + corruption_within +
      # Indicator variables don't need to be demeaned
      internal.conflict.past.5 + natural_disaster.occurrence +
      (1 | cowcode) + (1 | year),
    data=df)
}

mod.h1.barriers.new <- function(df) {
  lmer(
    total.oda_log_next_year ~ 
      barriers.total_new +
      polity_between + polity_within + 
      gdp.capita_log_between + gdp.capita_log_within +
      trade.pct.gdp_between + trade.pct.gdp_within + 
      corruption_between + corruption_within +
      # Indicator variables don't need to be demeaned
      internal.conflict.past.5 + natural_disaster.occurrence +
      (1 | cowcode) + (1 | year),
    data=df)
}

mod.h1.type.total <- function(df) {
  lmer(
    total.oda_log_next_year ~ 
      advocacy_between + advocacy_within +
      entry_between + entry_within +
      funding_between + funding_within +
      polity_between + polity_within + 
      gdp.capita_log_between + gdp.capita_log_within +
      trade.pct.gdp_between + trade.pct.gdp_within + 
      corruption_between + corruption_within +
      # Indicator variables don't need to be demeaned
      internal.conflict.past.5 + natural_disaster.occurrence +
      (1 | cowcode) + (1 | year),
    data=df)
}

mod.h1.type.new <- function(df) {
  lmer(
    total.oda_log_next_year ~ 
      advocacy_new + entry_new + funding_new +
      polity_between + polity_within + 
      gdp.capita_log_between + gdp.capita_log_within +
      trade.pct.gdp_between + trade.pct.gdp_within + 
      corruption_between + corruption_within +
      # Indicator variables don't need to be demeaned
      internal.conflict.past.5 + natural_disaster.occurrence +
      (1 | cowcode) + (1 | year),
    data=df)
}

mod.h1.csre <- function(df) {
  lmer(
    total.oda_log_next_year ~ 
      csre_between + csre_within +
      polity_between + polity_within + 
      gdp.capita_log_between + gdp.capita_log_within +
      trade.pct.gdp_between + trade.pct.gdp_within + 
      corruption_between + corruption_within +
      # Indicator variables don't need to be demeaned
      internal.conflict.past.5 + natural_disaster.occurrence +
      (1 | cowcode) + (1 | year),
    data=df)
}
