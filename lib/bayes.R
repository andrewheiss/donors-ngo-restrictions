library(rstanarm)
library(brms)
options(mc.cores = parallel::detectCores())  # Use all possible cores
# options(mc.cores = 1)  # Just use one core for now

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts") 
# So make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df$x) <- "contr.treatment"

CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
BAYES.SEED <- 1234


# In order for the models to converge correctly, variables with large values
# need to be rescaled. To interpret these models once they're done, the rescaled
# variables have to be un-rescaled back to their original levels. R has a
# scale() function that will rescale and/or center a vector, and it returns the
# scaling factors inside the object attributes "scaled:center" and "scaled:scale".
#
# my_scale() is a wrapper for scale() that returns a cleaner, easier-to-use
# version of scale attributes that we can access later when un-rescaling
my_scale <- function(x) {
  x_scaled <- scale(x, center = FALSE)
  x_scaled_numeric <- as.numeric(x_scaled)
  attributes(x_scaled_numeric) <- list(`scaled:scale` = 
                                         attributes(x_scaled)$`scaled:scale`)
  return(x_scaled_numeric)
}


diff.groups.bayes <- function(form, data, divide_by = 1) {
  form <- as.formula(form)
  df <- data
  
  # Run regression model
  model <- stan_glm(form, data = df, family = gaussian(),
                    prior = student_t(df = 1, location = 0),
                    prior_intercept = student_t(df = 1, location = 0),
                    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES.SEED)
  
  # MCMC draw for each group
  group.names <- levels(model$model[[attr(terms(form), "term.labels")]])
  
  samples.clean <- as.data.frame(model) %>%
    select(-sigma) %>%
    # The first column is the intercept/base case. Add it to all the other columns
    mutate_at(vars(-1), funs(. + `(Intercept)`)) %>%
    mutate_all(funs(. * divide_by)) %>%
    magrittr::set_colnames(group.names)
  
  samples.summary <- samples.clean %>%
    gather(group.name, value) %>%
    group_by(group.name) %>%
    summarise(mean = mean(value),
              median = median(value),
              q5 = quantile(value, probs = 0.05),
              q25 = quantile(value, probs = 0.25),
              q75 = quantile(value, probs = 0.75),
              q95 = quantile(value, probs = 0.95),
              p.greater0 = mean(value > 0)) %>%
    mutate(group.name = factor(group.name, levels = group.names, ordered = TRUE)) %>%
    arrange(group.name)
  
  # Return everything as a list
  output <- list(samples = samples.clean, samples.summary = samples.summary,
                 model = model)
  return(output)
}


bayes.meld <- function(models, coefs.to.keep, exponentiate = FALSE) {
  # Make big data frame of just posterior coefficients
  model.coefs <- models %>%
    mutate(model.df = model %>% map(~ as.data.frame(.))) %>%
    unnest(model.df) %>% 
    select(m, model.name, starts_with("b_"))
  
  model.coefs.long <- model.coefs %>% 
    gather(term, value, -m, -model.name) %>% 
    # There are a lot of missing values because not all models use the same
    # explanatory variables (i.e. CSRE is only in one model, so those columns
    # when wide are all NAs. When long, each value cell is also NA)
    filter(!is.na(value)) %>% 
    mutate(term = str_replace(term, "^b_", ""))
  
  model.scalings <- models %>% 
    unnest(scales)
  
  model.rescaled <- model.coefs.long %>% 
    filter(term %in% coefs.to.keep) %>% 
    left_join(model.scalings, by = c("m", "model.name", "term")) %>% 
    mutate(value_rescaled = ifelse(!is.na(scaled),
                                   value / scaled,
                                   value))
  
  model.coefs.summary <- model.rescaled %>% 
    filter(term %in% coefs.to.keep) %>% 
    group_by(model.name, term) %>%
    summarize(median = median(value_rescaled),
              mad = mad(value_rescaled))
  
  # Calculate the probability the mean is greater/less than zero
  model.posterior.probs <- model.rescaled %>% 
    group_by(model.name, term) %>% 
    summarize(p.greater0 = mean(value_rescaled > 0))
  
  # Generate credible intervals
  # terms have to be extracted from rownames in the posterior interval df
  model.credible.intervals <- models %>%
    mutate(post_pred = model %>% map(~ posterior_interval(., prob = 0.95) %>% 
                                       as.data.frame() %>%
                                       mutate(term = rownames(.)))) %>%
    unnest(post_pred) %>%
    mutate(term = str_replace(term, "^b_", "")) %>% 
    filter(term %in% coefs.to.keep) %>%
    left_join(model.scalings, by = c("m", "model.name", "term", "scales.name")) %>% 
    mutate_at(vars(contains("%")),
              funs(ifelse(!is.na(scaled), . / scaled, .))) %>% 
    group_by(model.name, term) %>%
    summarise_at(vars(contains("%")), median) %>%
    ungroup()
  
  melded.summary <- model.coefs.summary %>%
    left_join(model.credible.intervals, by = c("model.name", "term")) %>%
    left_join(model.posterior.probs, by = c("model.name", "term")) %>%
    rename(med = median) %>% 
    ungroup()
  
  if (exponentiate) {
    melded.summary <- melded.summary %>%
      mutate_at(vars(med, contains("%")), exp)
  }
  
  return(list(model.coefs = model.coefs, melded.summary = melded.summary))
}

# models = data frame of nested data frames and stanreg objects
bayesgazer <- function(models, digits = 2, caption = NULL, note = NULL, exponentiate = FALSE) {
  coefs.for.top <- models %>%
    unnest(tidy) %>%
    filter(str_detect(term, "^b_")) %>% 
    filter(!duplicated(term, fromLast = TRUE)) %>%
    mutate(term = str_replace(term, "^b_", "")) %>% 
    filter(term != "Intercept") %>% 
    pull(term) %>% 
    c(., "Intercept")
  
  coefs.for.bottom <- models %>% 
    unnest(tidy) %>% 
    filter(term == "sigma" | str_detect(term, "^sd_")) %>% 
    distinct(term) %>% 
    pull(term)
  
  model.order <- models %>%
    distinct(model.name) %>% pull(model.name)
  
  header.row <- function(header) {
    data_frame(term = paste0("**", header, "**"), 
               models = model.order, value = "") %>%
      spread(models, value)
  }
  
  note.row <- function(note) {
    crossing(term = note,
             models = model.order, value = "") %>%
      spread(models, value) %>%
      # Sort based on original note order
      slice(match(note, term))
  }
  
  model.melded <- bayes.meld(models, coefs.for.top, exponentiate = exponentiate)
  model.output <- model.melded$melded.summary

  fixed.coefs <- model.output %>%
    mutate(combined = paste0(fixed.digits(med, digits),
                             "\\ \n(",
                             fixed.digits(`2.5%`, digits), ", ", fixed.digits(`97.5%`, digits),
                             ")\\; ",
                             "*", fixed.digits(p.greater0, digits), "*")) %>%
    select(model.name, term, combined) %>%
    spread(model.name, combined) %>%
    mutate(term = factor(term, levels = coefs.for.top, ordered = TRUE)) %>% 
    arrange(term) %>% 
    mutate(term = as.character(term)) %>% 
    # Clean up term names
    mutate(term = str_replace(term, "TRUE$|FALSE$", ""),
           term = recode(term, `Intercept` = "Constant")) %>%
    left_join(coef.names.all, by = "term") %>%
    mutate(term_clean = ifelse(term == "Constant", "Constant", term_clean)) %>%
    select(-term) %>% rename(term = term_clean) %>%
    select_(.dots = c("term", model.order))
  
  random.coefs <- models %>% 
    unnest(tidy) %>% 
    filter(term %in% coefs.for.bottom) %>% 
    group_by(term, model.name) %>% 
    summarize(details = fixed.digits(median(estimate), digits)) %>% 
    ungroup() %>% 
    spread(model.name, details, fill = "") %>% 
    left_join(coef.names.all, by = "term") %>% 
    mutate(term = factor(term, levels = coefs.for.bottom, ordered = TRUE)) %>% 
    select_(.dots = c("term_clean", model.order)) %>%
    rename(term = term_clean)

  # Get glance() information for all imputed models even though pss and nobs are
  # identical across all imputations; averaging them just gets the actual value
  n.m <- models %>% distinct(m) %>% nrow()
  
  bottom.details <- models %>%
    unnest(glance) %>%
    group_by(model.name) %>%
    summarise_at(vars(pss, nobs), mean) %>%
    mutate(m = n.m) %>%
    gather(term, value, -model.name) %>%
    mutate(value = as.character(value)) %>%
    spread(model.name, value) %>%
    left_join(coef.names.all, by = "term") %>%
    select_(.dots = c("term_clean", model.order)) %>%
    rename(term = term_clean)
  
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
    select_(.dots = c("term", model.order))
  
  # Make column names (1), (2), etc.
  # TODO: Allow for custom column names
  colnames(nice.top.bottom) <- c(" ", paste0("(", 1:length(model.order), ")"))
  
  # All columns are centered except the first
  # TODO: MAYBE: Let this be user configurable
  table.align <- paste0(c("l", rep("c", length(model.order))), collapse = "")
  
  pandoc.table.return(nice.top.bottom, keep.line.breaks = TRUE,
                      missing = "",
                      justify = table.align, caption = caption)
}

generate_predictions <- function(x, draws = 10) {
  predict(model, newdata = x, summary = FALSE, nsamples = draws, re_formula = NA) %>%
    apply(., 2, median)
}
