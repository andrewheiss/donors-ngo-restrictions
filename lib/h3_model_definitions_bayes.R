# Logit-transformed DV models
form.h3.dom.barriers.total <- prop.ngo.dom_logit_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.dom.type.total <- prop.ngo.dom_logit_next_year ~ 
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

form.h3.dom.csre <- prop.ngo.dom_logit_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

# Proportion-based zoib models
form.h3.dom.barriers.total.zoib <- prop.ngo.dom_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.dom.type.total.zoib <- prop.ngo.dom_next_year ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.dom.csre.zoib <- prop.ngo.dom_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.dom +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)


h3.dom.formulas <- c(form.h3.dom.barriers.total, 
                     form.h3.dom.type.total, 
                     form.h3.dom.csre)
h3.dom.rhs <- h3.dom.formulas %>% map(get.rhs) %>% unlist() %>% unique()
h3.dom.lhs <- h3.dom.formulas %>% map(get.lhs) %>% unlist() %>% unique()

h3.dom.formulas.zoib <- c(form.h3.dom.barriers.total.zoib, 
                          form.h3.dom.type.total.zoib, 
                          form.h3.dom.csre.zoib)
h3.dom.rhs.zoib <- h3.dom.formulas.zoib %>% map(get.rhs) %>% unlist() %>% unique()
h3.dom.lhs.zoib <- h3.dom.formulas.zoib %>% map(get.lhs) %>% unlist() %>% unique()


# Logit-transformed models
mod.h3.dom.barriers.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.barriers.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.barriers.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.dom.type.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.type.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.type.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.dom.csre.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.csre.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.csre),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

# Zero-one inflated beta models
mod.h3.dom.barriers.total.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.barriers.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.barriers.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.dom.type.total.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.type.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.type.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.dom.csre.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.dom.csre.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.dom.csre.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Foreign proportion models

# Logit-transformed DV models
form.h3.foreign.barriers.total <- prop.ngo.foreign_logit_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.foreign.type.total <- prop.ngo.foreign_logit_next_year ~ 
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

form.h3.foreign.csre <- prop.ngo.foreign_logit_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign_logit +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

# Proportion-based zoib models
form.h3.foreign.barriers.total.zoib <- prop.ngo.foreign_next_year ~ 
  barriers.total_within + barriers.total_between +
  polity_within + polity_between +
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.foreign.type.total.zoib <- prop.ngo.foreign_next_year ~ 
  advocacy_within + advocacy_between +
  entry_within + entry_between +
  funding_within + funding_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)

form.h3.foreign.csre.zoib <- prop.ngo.foreign_next_year ~ 
  csre_within + csre_between +
  polity_within + polity_between + 
  gdp.capita_log_within + gdp.capita_log_between +
  trade.pct.gdp_within + trade.pct.gdp_between +
  corruption_within + corruption_between +
  prop.ngo.foreign +
  internal.conflict.past.5 + natural_disaster.occurrence +
  (1 | cowcode) + (1 | year)


h3.foreign.formulas <- c(form.h3.foreign.barriers.total, 
                         form.h3.foreign.type.total, 
                         form.h3.foreign.csre)
h3.foreign.rhs <- h3.foreign.formulas %>% map(get.rhs) %>% unlist() %>% unique()
h3.foreign.lhs <- h3.foreign.formulas %>% map(get.lhs) %>% unlist() %>% unique()

h3.foreign.formulas.zoib <- c(form.h3.foreign.barriers.total.zoib, 
                              form.h3.foreign.type.total.zoib, 
                              form.h3.foreign.csre.zoib)
h3.foreign.rhs.zoib <- h3.foreign.formulas.zoib %>% map(get.rhs) %>% unlist() %>% unique()
h3.foreign.lhs.zoib <- h3.foreign.formulas.zoib %>% map(get.lhs) %>% unlist() %>% unique()


# Logit-transformed models
mod.h3.foreign.barriers.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.barriers.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.barriers.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.foreign.type.total.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.type.total.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.type.total),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.foreign.csre.bayes <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.csre.bayes \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.csre),
      data = df, family = gaussian(), inits = 0,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 2.5)", class = "b"),
                set_prior("normal(0, 2.5)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}


# Zero-one inflated beta models
mod.h3.foreign.barriers.total.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.barriers.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.barriers.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.foreign.type.total.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.type.total.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.type.total.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}

mod.h3.foreign.csre.bayes.zoib <- function(df) {
  cat("#####\n")
  cat("Running model for mod.h3.foreign.csre.bayes.zoib \n")
  cat("#####\n")
  
  brm(brmsformula(form.h3.foreign.csre.zoib, coi ~ 1, zoi ~ 1),
      data = df, family = zero_one_inflated_beta(), 
      prior = c(set_prior("normal(0, 5)", class = "Intercept"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "zoi"),
                set_prior("normal(0, 0.5)", class = "Intercept", dpar = "coi"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("exponential(1)", class = "sd")),
      chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
}
