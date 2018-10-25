# Logit-transformed DV models
form.h2.barriers.total <- prop.contentious_logit_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious_logit +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

form.h2.type.total <- prop.contentious_logit_next_year ~ 
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

form.h2.csre <- prop.contentious_logit_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious_logit +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)


# Proportion-based zoib models
form.h2.barriers.total.zoib <- prop.contentious_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

form.h2.type.total.zoib <- prop.contentious_next_year ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)

form.h2.csre.zoib <- prop.contentious_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.contentious +
  internal.conflict.past.5 + natural_disaster.occurrence + post.1989 +
  (1 | cowcode) + (1 | year)


h2.formulas <- c(form.h2.barriers.total, form.h2.type.total, form.h2.csre)
h2.rhs <- h2.formulas %>% map(get.rhs) %>% unlist() %>% unique()
h2.lhs <- h2.formulas %>% map(get.lhs) %>% unlist() %>% unique()

h2.formulas.zoib <- c(form.h2.barriers.total.zoib, form.h2.type.total.zoib, form.h2.csre.zoib)
h2.rhs.zoib <- h2.formulas.zoib %>% map(get.rhs) %>% unlist() %>% unique()
h2.lhs.zoib <- h2.formulas.zoib %>% map(get.lhs) %>% unlist() %>% unique()


# Logit-transformed models
mod.h2.barriers.total.bayes <- function(df) { 
  cat("#####\n")
  cat("Running model for mod.h2.barriers.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h2.barriers.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.type.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h2.type.total.bayes \n")
  cat("#####\n")

  brm(brmsformula(form.h2.type.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.csre.bayes <- function(df) {  
  cat("#####\n")
  cat("Running model for mod.h2.csre.bayes \n")
  cat("#####\n")

  brm(brmsformula(form.h2.csre),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

# Zero-one inflated beta models
mod.h2.barriers.total.bayes.zoib <- function(df) { 
  cat("#####\n")
  cat("Running model for mod.h2.barriers.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h2.barriers.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.type.total.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h2.type.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h2.type.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h2.csre.bayes.zoib <- function(df) {  
  cat("#####\n")
  cat("Running model for mod.h2.csre.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h2.csre.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
