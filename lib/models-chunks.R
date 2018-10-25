# ---- load-libraries ---------------------------------------------------------
knitr::opts_chunk$set(cache = FALSE, fig.retina = 2,
                      tidy.opts = list(width.cutoff = 120),  # For code
                      options(width = 120))  # For output

library(tidyverse)
library(stringr)
library(forcats)
library(stargazer)
library(huxtable)
library(lme4)
library(modelr)
library(broom)
library(broom.mixed)
library(scales)
library(formula.tools)
library(here)

source(here("lib", "graphics.R"))
source(here("lib", "pandoc.R"))
source(here("lib", "bayes.R"))

source(here("lib", "robustness_models_definitions.R"))
source(here("lib", "h1_model_definitions.R"))
source(here("lib", "h2_model_definitions.R"))
source(here("lib", "h3_model_definitions.R"))

my.seed <- 1234
set.seed(my.seed)


# ---- load-data --------------------------------------------------------------
df.country.aid <- readRDS(here("Data", "data_clean",
                               "df_country_aid_no_imputation.rds"))

df.country.aid.impute <- readRDS(here("Data", "data_clean",
                                      "df_country_aid_imputation.rds"))

df.country.aid.impute.m10 <- readRDS(here("Data", "data_clean",
                                          "df_country_aid_imputation_m10.rds"))

dcjw.questions.clean <- read_csv(here("Data", "data_manual", "dcjw_questions.csv"))
dcjw.responses.clean <- read_csv(here("Data", "data_manual", "dcjw_responses.csv"))

# Load clean coefficient names and append "within" and "between" to them,
# resulting in a giant table of possible coefficient names
coef.names <- read_csv(here("Data", "data_manual", "coef_names.csv"))

coef.names.within <- coef.names %>%
  mutate(term = paste0(term, "_within"),
         term_plot = paste0(term_clean, "\n(within)"),
         term_plot_short = term_clean,
         term_clean = paste0(term_clean, "~within~"))

coef.names.between <- coef.names %>%
  mutate(term = paste0(term, "_between"),
         term_plot = paste0(term_clean, "\n(between)"),
         term_plot_short = paste0(term_clean, ""),
         term_clean = paste0(term_clean, "~between~"))

coef.names.all <- bind_rows(coef.names, coef.names.within, coef.names.between) %>%
  mutate_at(vars(term_plot, term_plot_short),
            funs(ifelse(is.na(.), term_clean, .))) %>% 
  mutate(term_plot_short = recode(term_plot_short,
                                  `Civil society reg. env. (CSRE)` = "CSRE"))

# Load clean model names
model.names <- read_csv(here("Data", "data_manual", "model_names.csv"))


# Combine original data and imputed data so calculations can happen at the same time
df.country.aid.both <- bind_rows(df.country.aid, df.country.aid.impute)

# Combine m=10 imputed data too since we use it in some robustness checks.
# Imputations 6-10 are later removed
df.country.aid.all <- bind_rows(df.country.aid, df.country.aid.impute.m10)

# All the missing values have been taken care of, but final years for leaded
# variables *are* still missing (i.e. there's no aid data for 2014, so
# total.oda_log_next_year will be NA in 2013). For this fancy random effects
# regression to work, the demeaned variables have to be based on the means of
# all rows included in the regression, so they can't include rows that are
# dropped because of missingness. This means we have to make separate demeaned
# datasets for *_after_2 and *_after_5, but ¯\_(ツ)_/¯
df.country.aid.demean.next_year.all <- df.country.aid.all %>%
  filter(!is.na(total.oda_log_next_year)) %>%
  group_by(m, cowcode) %>%
  mutate_at(vars(barriers.total, advocacy, entry, funding, 
                 polity, gdp.capita_log, gdp.capita, trade.pct.gdp, corruption, csre,
                 total.oda_log),
            funs(between = mean(., na.rm = TRUE),  # meaned
                 within = . - mean(., na.rm = TRUE))) %>%  # demeaned
  ungroup()

df.country.aid.us.demean.next_year.all <- df.country.aid.all %>%
  filter(!is.na(prop.ngo.dom_logit_next_year)) %>%
  filter(year > 1999) %>%
  group_by(m, cowcode) %>%
  mutate_at(vars(barriers.total, advocacy, entry, funding, 
                 polity, gdp.capita_log, gdp.capita, trade.pct.gdp, corruption, csre,
                 total.oda_log),
            funs(between = mean(., na.rm = TRUE),  # meaned
                 within = . - mean(., na.rm = TRUE))) %>%  # demeaned
  ungroup()

# Divide demeaned data into separate data frames: original, imputed (m=5), and imputed (m=10)
df.country.aid.demean.next_year.both <- 
  filter(df.country.aid.demean.next_year.all, !(m %in% paste0("imp", 6:10)))

df.country.aid.demean.next_year <- 
  filter(df.country.aid.demean.next_year.all, m == "original")

df.country.aid.us.demean.next_year.both <- 
  filter(df.country.aid.us.demean.next_year.all, !(m %in% paste0("imp", 6:10)))

df.country.aid.us.demean.next_year <- 
  filter(df.country.aid.us.demean.next_year.all, m == "original")

df.country.aid.demean.next_year.impute <- 
  filter(df.country.aid.demean.next_year.all, 
         m != "original", !(m %in% paste0("imp", 6:10)))

df.country.aid.demean.next_year.impute.m10 <- 
  filter(df.country.aid.demean.next_year.all, m != "original")

# Demean data with total.oda_log leaded by 2 years and 5 years
# After 2 years
df.country.aid.demean.after_2.both <- df.country.aid.both %>%
  filter(!is.na(total.oda_log_after_2)) %>%
  group_by(m, cowcode) %>%
  mutate_at(vars(barriers.total, advocacy, entry, funding, 
                 polity, gdp.capita_log, gdp.capita, trade.pct.gdp, corruption, csre,
                 total.oda_log),
            funs(between = mean(., na.rm = TRUE),  # meaned
                 within = . - mean(., na.rm = TRUE))) %>%  # demeaned
  ungroup()

df.country.aid.demean.after_2 <- 
  filter(df.country.aid.demean.after_2.both, m == "original")

df.country.aid.demean.after_2.impute <- 
  filter(df.country.aid.demean.after_2.both, m != "original")

# After 5 years
df.country.aid.demean.after_5.both <- df.country.aid.both %>%
  filter(!is.na(total.oda_log_after_5)) %>%
  group_by(m, cowcode) %>%
  mutate_at(vars(barriers.total, advocacy, entry, funding, 
                 polity, gdp.capita_log, gdp.capita, trade.pct.gdp, corruption, csre,
                 total.oda_log),
            funs(between = mean(., na.rm = TRUE),  # meaned
                 within = . - mean(., na.rm = TRUE))) %>%  # demeaned
  ungroup()

df.country.aid.demean.after_5 <- 
  filter(df.country.aid.demean.after_5.both, m == "original")

df.country.aid.demean.after_5.impute <- 
  filter(df.country.aid.demean.after_5.both, m != "original")


# ---- helpful-functions ------------------------------------------------------
stars <- function(p) {
  out <- symnum(p, cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "**", "*", ""))
  as.character(out)
}

fixed.digits <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

# Use 2 significant digits only on the decimal part of the number, ignoring the
# integer part. See my question here: http://stackoverflow.com/q/43050903/120898
# fixed.digits <- function(x, digits = 2) {
#   as.character(floor(x) + signif(x %% 1, digits))
# }

# Inverse logit, with the ability to account for adjustments
# via http://stackoverflow.com/a/23845527/120898
inv.logit <- function(f, a) {
  a <- (1 - 2 * a)
  (a * (1 + exp(f)) + (exp(f) - 1)) / (2 * a * (1 + exp(f)))
}

# Take apart the pieces of a random effects formula and rebuild it
build.formula <- function(DV, IVs) {
  terms.all <- attr(terms(IVs), "term.labels")
  terms.fixed <- terms.all[!stringr::str_detect(terms.all, "\\|")]
  terms.rand <- sapply(findbars(formula(IVs)),function(x) paste0("(", deparse(x), ")"))
  
  reformulate(c(terms.fixed, terms.rand), response = DV)
}

get.rhs <- function(x) rhs.vars(x) %>% str_replace_all("1 \\| ", "")
get.lhs <- function(x) lhs.vars(x)
is_scaled <- function(x) "scaled:scale" %in% names(attributes(x))
get_scale <- function(x) attr(x, "scaled:scale")


# Meld a bunch of imputed models
meld.imputed.models <- function(model.data, exponentiate = FALSE) {
  models.df <- model.data$glance[[1]]$df.residual
  
  models.tidy <- model.data %>%
    select(tidy) %>%
    unnest(.id = "imputation")
  
  just.estimates <- models.tidy %>% 
    filter(group == "fixed") %>%
    select(imputation, term, estimate) %>%
    spread(term, estimate) %>%
    select(-imputation)
  
  just.ses <- models.tidy %>%
    filter(group == "fixed") %>%
    select(imputation, term, std.error) %>%
    spread(term, std.error) %>%
    select(-imputation)
  
  # If no imputed data was passed in, use the actual estimates and SEs
  if (nrow(just.estimates) > 1) {
    melded <- Amelia::mi.meld(just.estimates, just.ses)
  } else {
    melded <- list(q.mi = just.estimates, se.mi = just.ses)
  }
  
  melded.tidy <- as.data.frame(cbind(t(melded$q.mi), 
                                     t(melded$se.mi))) %>%
    magrittr::set_colnames(c("estimate", "std.error")) %>%
    mutate(term = rownames(.)) %>%
    select(term, everything()) %>%
    mutate(statistic = estimate / std.error,
           conf.low = estimate + std.error * qt(0.025, models.df),
           conf.high = estimate + std.error * qt(0.975, models.df),
           p.value = 2 * pt(abs(statistic), models.df, lower.tail = FALSE),
           stars = stars(p.value))
  
  if (exponentiate) {
    # Convert SEs to odds ratios. This isn't entirely 100% accurate, since the
    # melded coefficient variances (i.e. diag(vcov(.))) are just averaged, not
    # melded with Amelia's fancy mi.meld(), but ¯\_(ツ)_/¯
    #
    # https://www.andrewheiss.com/blog/2016/04/25/convert-logistic-regression-standard-errors-to-odds-ratios-with-r/
    fixed.coefs.var <- model.data %>%
      mutate(var.diag = model %>% map(~ diag(vcov(.)))) %>%
      select(var.diag, model) %>% unnest(var.diag)
    
    just.var <- models.tidy %>%
      filter(group == "fixed") %>%
      select(imputation, term) %>%
      bind_cols(fixed.coefs.var) %>%
      group_by(term) %>%
      summarise(var.diag = mean(var.diag))
    
    melded.tidy <- melded.tidy %>%
      left_join(just.var, by = "term") %>%
      mutate(or = exp(estimate),
             or.se = sqrt(or^2 * var.diag),
             or.upper = or + (qnorm(0.975) * or.se),
             or.lower = or + (qnorm(0.025) * or.se))
  }
  
  melded.tidy
}

# Expects a data frame with a column named tidy.melded and a row for model
# names. Term names are based on the first model in the data column for each
# row, and are filtered through stargazer to get the correct row order.
stargazer.fake <- function(df, caption = NULL, note = NULL, exponentiate = FALSE) {
  # Create a blank row with a bolded row name
  header.row <- function(header) {
    data_frame(term = paste0("**", header, "**"), 
               models = df$model.name, value = "") %>%
      spread(models, value)
  }
  
  note.row <- function(note) {
    crossing(term = note,
             models = df$model.name, value = "") %>%
      spread(models, value) %>%
      # Sort based on original note order
      slice(match(note, term))
  }
  
  coef.order.models <- df %>%
    # Select just the first row of each model
    unnest(data) %>%
    # Sometimes there are duplicate column names
    # magrittr::set_colnames(make.unique(colnames(.))) %>%
    # Keep order of model.name
    mutate(model.name = ordered(fct_inorder(model.name))) %>%
    group_by(model.name) %>%
    slice(1) %>% ungroup() %>%
    select(model) %>% as.list()
  
  # Use stargazer to get the coefficient order. I tried recreating stargazer's
  # coefficient ordering algorithm but it's way too complicated. So instead, we
  # cheat and let stargazer do the heavy lifting, save the output to a string,
  # and then extract the coefficient names with str_extract. Super super hacky,
  # but it works.
  #
  # See http://stackoverflow.com/a/41801861/120898
  capture.output({
    stargazer.coefs <- stargazer::stargazer(coef.order.models, 
                                            type = "text", table.layout = "t")
  }, file = "/dev/null")
  
  coef.order <- setdiff(stringr::str_extract(stargazer.coefs, "^[\\w\\.]*"), c(""))
  
  # Fixed parts
  fixed.tidy <- df %>%
    unnest(tidy.melded)
  
  if (exponentiate) {
    fixed.coefs <- fixed.tidy %>%
      mutate(fancy = paste0(fixed.digits(or, 3),
                            stars, "\\ \n(",
                            fixed.digits(or.se, 3),
                            ")"))
  } else {
    fixed.coefs <- fixed.tidy %>%
      mutate(fancy = paste0(fixed.digits(estimate, 3),
                            stars, "\\ \n(",
                            fixed.digits(std.error, 3),
                            ")"))
  }
  
  fixed.coefs <- fixed.coefs %>%
    select(model.name, term, fancy) %>%
    spread(model.name, fancy, fill = "") %>%
    # Clean up term names
    mutate(term = stringr::str_replace(term, "TRUE$|FALSE$", ""),
           term = recode(term, `(Intercept)` = "Constant")) %>%
    # Use stargazer's coefficient order
    mutate(term = factor(term, levels = coef.order, ordered = TRUE)) %>%
    arrange(term) %>%
    mutate(term = as.character(term)) %>%
    left_join(coef.names.all, by = "term") %>%
    mutate(term_clean = ifelse(term == "Constant", "Constant", term_clean)) %>%
    select(-term) %>% rename(term = term_clean) %>%
    select_(.dots = c("term", df$model.name))
  
  # Random parts
  random.coef.order.raw <- df %>%
    unnest(data) %>%
    # unnest(data) %>% magrittr::set_colnames(make.unique(colnames(.))) %>%
    select(model.name, model) %>%
    mutate(ranef = model %>% map(~ as.data.frame(VarCorr(.)))) %>%
    unnest(ranef)
  random.coef.order <- c(setdiff(unique(random.coef.order.raw$grp), "Residual"), "Residual")
  
  random.coefs <- df %>%
    unnest(data) %>%
    unnest(tidy) %>%
    filter(group != "fixed") %>%
    rename(term.raw = term, term = group) %>%
    group_by(model.name, term) %>%
    summarise(avg.random.sd = mean(estimate),
              sd.random.sd = sd(estimate)) %>%
    mutate(fancy = paste0(fixed.digits(avg.random.sd, 3),
                          "\\ \n(",
                          ifelse(is.na(sd.random.sd), "NA", fixed.digits(sd.random.sd, 3)),
                          ")")) %>%
    select(model.name, term, fancy) %>%
    spread(model.name, fancy, fill = "") %>%
    mutate(term = factor(term, levels = random.coef.order, ordered = TRUE)) %>%
    arrange(term) %>% mutate(term = as.character(term)) %>%
    left_join(coef.names.all, by = "term") %>%
    mutate(term_clean = ifelse(term == "Residual", 
                               "Residual random error ($\\sigma$)", term_clean)) %>%
    select(-term) %>% select(term = term_clean, everything())
  
  # Create the bottom half of the table
  # Calculate the average log likelihood for all imputed models
  avg.loglik <- df %>%
    unnest(data) %>% unnest(glance) %>%
    group_by(model.name) %>%
    summarise(avg.loglik = as.character(round(mean(logLik), 2))) %>%
    mutate(term = "Log likelihood (mean)") %>%
    spread(model.name, avg.loglik)
  
  # Imputation frames
  n.obs <- df %>%
    unnest(data) %>%
    mutate(n = model %>% map_int(~ nrow(.@frame))) %>%
    select(model.name, n) %>%
    group_by(model.name) %>% slice(1) %>% ungroup() %>%
    mutate(term = "Observations",
           n = scales::comma(n)) %>%
    spread(model.name, n)
  
  n.m <- df %>%
    mutate(m.new = data %>% map_int(~ nrow(.))) %>%
    select(model.name, m.new) %>% 
    mutate(m.new = ifelse(m.new == 1, 0, m.new),
           m.new = as.character(m.new)) %>%
    mutate(term = "Imputed datasets (*m*)") %>%
    spread(model.name, m.new)
  
  bottom.details <- bind_rows(n.obs, avg.loglik, n.m)
  
  if (exponentiate) {
    fixed.title <- "Fixed part (odds ratios)"
    random.title <- "Random part (original coefficients)"
  } else {
    fixed.title <- "Fixed part"
    random.title <- "Random part"
  }
  
  if (!is.null(note)) {
    notes <- bind_rows(header.row("Notes"), note.row(note))
  } else {
    notes <- NULL
  }
  
  nice.top.bottom <- bind_rows(header.row(fixed.title), fixed.coefs, 
                               header.row(random.title), random.coefs,
                               header.row("Model details"), bottom.details,
                               notes) %>%
    select_(.dots = c("term", df$model.name))
  
  # Make column names (1), (2), etc.
  # TODO: Allow for custom column names
  colnames(nice.top.bottom) <- c(" ", paste0("(", 1:length(df$model.name), ")"))
  
  # All columns are centered except the first
  # TODO: MAYBE: Let this be user configurable
  table.align <- paste0(c("l", rep("c", length(df$model.name))), collapse = "")
  
  pandoc.table.return(nice.top.bottom, keep.line.breaks = TRUE,
                      justify = table.align, caption = caption)
}
