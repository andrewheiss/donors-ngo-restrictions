form.h1.barriers.total <- total.oda_log_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  total.oda_log +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

form.h1.type.total <- total.oda_log_next_year ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  total.oda_log +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

form.h1.csre <- total.oda_log_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  total.oda_log +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)


h1.formulas <- c(form.h1.barriers.total, form.h1.type.total, form.h1.csre)
h1.rhs <- h1.formulas %>% map(get.rhs) %>% unlist() %>% unique()
h1.lhs <- h1.formulas %>% map(get.lhs) %>% unlist() %>% unique()


mod.h1.barriers.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h1.barriers.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h1.barriers.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h1.type.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h1.type.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h1.type.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h1.csre.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h1.csre.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h1.csre),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
