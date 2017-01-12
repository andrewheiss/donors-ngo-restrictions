library(rstanarm)
options(mc.cores = parallel::detectCores())  # Use all possible cores

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
