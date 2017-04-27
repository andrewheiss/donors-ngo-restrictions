library(rstanarm)
# options(mc.cores = parallel::detectCores())  # Use all possible cores
options(mc.cores = 1)  # Just use one core for now

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts") 
# So make ordered factors use treatment contrasts instead
options(contrasts=rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df$x) <- "contr.treatment"

CHAINS <- 4
ITER <-2000
WARMUP <- 1000
BAYES.SEED <- 1234

diff.groups.bayes <- function(form, data, divide_by=1) {
  form <- as.formula(form)
  df <- data
  
  # Run regression model
  model <- stan_glm(form, data=df, family=gaussian(),
                    prior=student_t(df=1, location=0),
                    prior_intercept=student_t(df=1, location=0),
                    chains=CHAINS, iter=ITER, warmup=WARMUP, seed=BAYES.SEED)
  
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
              q2.5 = quantile(value, probs=0.025),
              q25 = quantile(value, probs=0.25),
              q75 = quantile(value, probs=0.75),
              q97.5 = quantile(value, probs=0.975),
              p.greater0 = mean(value > 0)) %>%
    mutate(group.name = factor(group.name, levels=group.names, ordered=TRUE)) %>%
    arrange(group.name)
  
  # Return everything as a list
  output <- list(samples=samples.clean, samples.summary=samples.summary,
                 model=model)
  return(output)
}

# THIS IS SO HORRIBLY HACKY AND AWFUL BUT ALSO KIND OF AMAZING.
#
# As discovered here, http://stackoverflow.com/a/41801861/120898, there's no
# easy way to replicate stargazer's coefficient ordering logic. But because
# stargazer returns the resulting character vector, it's easy enough to capture
# the string and extract the coefficients.
#
# But stargazer doesn't support stanreg objects. So this function extracts the
# formula terms from a stanreg object, uses those names to create completely
# fake data, and returns a simple lm based on the fake data. Then later, in the
# bayesgazer function, the correct coefficient order can be derived based on
# these fake models.
fake.lm <- function(form) {
  form <- as.character(form)
  
  # Extract variables from the formula
  dv <- form[2]
  ivs <- form[3] %>% str_split("\\+") %>% unlist() %>% str_trim() %>%
    Filter(function(x) !str_detect(x, "\\|"), .)  # ignore (1 | x) terms
  
  # Make a simplified, clean formula
  form.new <- as.formula(paste0(dv, " ~ ", paste(ivs, collapse=" + ")))
  
  # Create fake dataset
  N <- 100
  M <- length(c(dv, ivs))
  totally.fake.data <- matrix(rnorm(N * M), N, M) %>%
    as.data.frame() %>%
    magrittr::set_colnames(c(dv, ivs))
  
  # Model with fake data
  model.fake <- lm(form.new, data=totally.fake.data)
  return(model.fake)
}

bayes.meld <- function(models, coefs.to.keep, exponentiate=FALSE) {
  model.coefs <- models %>%
    # Make big data frame of just posterior coefficients
    # For some reason one of the Sigma columns really likes to duplicate itself, 
    # so force the columns to be unique
    mutate(model.df = model %>% map(~ as.data.frame(.) %>%
                                      magrittr::set_colnames(make.unique(colnames(.))))) %>%
    unnest(model.df)
  
  model.coefs.summary <- model.coefs %>%
    # Only keep variables in coefs.to.keep
    select(model.name, one_of(coefs.to.keep)) %>%
    group_by(model.name) %>%
    summarise_all(.funs=funs(XXXmedian = median(.), XXXmad = mad(.))) %>%
    gather(variable, value, -model.name) %>%
    separate(variable, c("term", "measure"), sep="_XXX") %>%
    spread(measure, value) %>%
    filter(!is.na(median))
  
  # Calculate the probability the mean is greater/less than zero
  model.posterior.probs <- model.coefs %>%
    # Only keep variables in coefs.to.keep
    select(model.name, one_of(coefs.to.keep)) %>%
    group_by(model.name) %>%
    summarise_all(funs(pp.greater0 = mean(. > 0))) %>%
    gather(key, value, -model.name) %>%
    separate(key, c("term", "key"), sep="_p") %>%
    spread(key, value) %>%
    filter(!is.na(p.greater0))
  
  # Generate credible intervals
  # terms have to be extracted from rownames in the posterior interval df
  model.credible.intervals <- models %>%
    mutate(post_pred = model %>% map(~ posterior_interval(., prob=0.95) %>% 
                                       as.data.frame() %>%
                                       mutate(term = rownames(.)))) %>%
    unnest(post_pred) %>%
    filter(term %in% coefs.to.keep) %>%
    group_by(model.name, term) %>%
    summarise_at(vars(`2.5%`, `97.5%`), mean) %>%
    ungroup()
  
  melded.summary <- model.coefs.summary %>%
    left_join(model.credible.intervals, by=c("model.name", "term")) %>%
    left_join(model.posterior.probs, by=c("model.name", "term")) %>%
    rename(med = median)
  
  if (exponentiate) {
    melded.summary <- melded.summary %>%
      mutate_at(vars(med, `2.5%`, `97.5%`), exp)
  }
  
  return(list(model.coefs = model.coefs, melded.summary = melded.summary))
}

# models = data frame of nested data frames and stanreg objects
bayesgazer <- function(models, digits=2, caption=NULL, note=NULL, exponentiate=FALSE,
                       coefs.for.bottom = c("cowcode_b", "year_b", "sigma_b")) {
  coefs.for.top <- models %>%
    unnest(tidy) %>%
    distinct(term) %>% unlist() %>% c() %>% unname()
  
  # Determine coefficient order using stargazer
  fake.models <- models %>%
    filter(m == "imp1") %>%
    mutate(fake.model = model %>% map(~ fake.lm(.$form))) %>%
    select(fake.model) %>% as.list()
  
  capture.output({
    stargazer.coefs <- stargazer::stargazer(fake.models, 
                                            type="text", table.layout="t")
  }, file="/dev/null")
  
  coef.order <- setdiff(stringr::str_extract(stargazer.coefs, "^[\\w\\.]*"), c(""))
  
  model.order <- models %>%
    distinct(model.name) %>% unlist() %>% c() %>% unname()
  
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
  
  model.melded <- bayes.meld(models, coefs.for.top, exponentiate=exponentiate)
  
  model.output <- model.melded$melded.summary
  model.coefs <- model.melded$model.coefs
  
  fixed.coefs <- model.output %>%
    mutate(combined = paste0(fixed.digits(med, digits),
                             "\\ \n(",
                             fixed.digits(`2.5%`, digits), ", ", fixed.digits(`97.5%`, digits),
                             ")\\ \n",
                             "*", fixed.digits(p.greater0, digits), "*")) %>%
    select(model.name, term, combined) %>%
    spread(model.name, combined) %>%
    # Clean up term names
    mutate(term = stringr::str_replace(term, "TRUE$|FALSE$", ""),
           term = recode(term, `(Intercept)` = "Constant")) %>%
    # Use stargazer's coefficient order
    mutate(term = factor(term, levels=coef.order, ordered=TRUE)) %>%
    arrange(term) %>%
    mutate(term = as.character(term)) %>%
    left_join(coef.names.all, by="term") %>%
    mutate(term_clean = ifelse(term == "Constant", "Constant", term_clean)) %>%
    select(-term) %>% rename(term = term_clean) %>%
    select_(.dots = c("term", model.order))
  
  random.coefs.raw <- model.coefs %>%
    select(-m, -contains("b[(Intercept)"), -one_of(coefs.for.top)) %>%
    group_by(model.name) %>%
    summarise_all(funs(fixed.digits(median(.), digits)))
  
  # If the model has both cowcode and year random effects, rename the columns
  # accordingly
  if (all(c("cowcode_b", "year_b") %in% coefs.for.bottom)) {
    random.coefs.renamed <- random.coefs.raw %>%
      select(model.name, sigma_b = sigma,
             cowcode_b = `Sigma[(Intercept),(Intercept)]`,
             year_b = `Sigma[(Intercept),(Intercept)].1`)
  } else {
    random.coefs.renamed <- random.coefs.raw %>%
      select(model.name, sigma_b = sigma,
             year_b = `Sigma[(Intercept),(Intercept)]`)
  }

  random.coefs <- random.coefs.renamed %>%
    ungroup() %>%
    gather(term, value, -model.name) %>%
    spread(model.name, value, fill="") %>%
    mutate(term = factor(term, levels=coefs.for.bottom, ordered=TRUE)) %>%
    arrange(term) %>% mutate(term = as.character(term)) %>%
    left_join(coef.names.all, by="term") %>%
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
    left_join(coef.names.all, by="term") %>%
    select_(.dots = c("term_clean", model.order)) %>%
    rename(term = term_clean)
  
  # Calculate the median and MAD_SD for the posterior predictive distribution
  # of y (the bottom part of summary(model) output)
  post.bottom <- models %>%
    mutate(model.fit.df = model %>% map(~ as.data.frame(.$stanfit))) %>%
    unnest(model.fit.df) %>%
    group_by(model.name) %>%
    summarise(Median = median(mean_PPD),
              `Median absolute deviation (SD)` = mad(mean_PPD)) %>%
    gather(term, value, -model.name) %>%
    mutate(value = fixed.digits(value, digits)) %>%
    spread(model.name, value) %>%
    select_(.dots = c("term", model.order))
  
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
                               note.row("Sample average posterior\\\npredictive distribution of y ($X = \\bar{x}$):"),
                               post.bottom, notes) %>%
    select_(.dots = c("term", model.order))
  
  # Make column names (1), (2), etc.
  # TODO: Allow for custom column names
  colnames(nice.top.bottom) <- c(" ", paste0("(", 1:length(model.order), ")"))
  
  # All columns are centered except the first
  # TODO: MAYBE: Let this be user configurable
  table.align <- paste0(c("l", rep("c", length(model.order))), collapse="")
  
  pandoc.table.return(nice.top.bottom, keep.line.breaks=TRUE,
                      missing="",
                      justify=table.align, caption=caption)
}

generate_predictions <- function(x, draws=10) {
  rstanarm::posterior_predict(model, newdata=x, draws=draws, re.form=NA) %>%
    as.data.frame() %>%
    summarise_all(median) %>%
    gather(key, predicted) %>% 
    select(predicted) %>% unlist() %>% unname() %>% c()
}
