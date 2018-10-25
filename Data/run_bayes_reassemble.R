# Each model was run indiviudally in run_bayes_remote.R because it takes so long. 
# Here we reassemble all the pieces.
library(tidyverse)
library(here)

# Make a data frame of just the scale factors for each of the variables that we rescaled
is_scaled <- function(x) "scaled:scale" %in% names(attributes(x))
get_scale <- function(x) attr(x, "scaled:scale")

get_scales <- function(df) {
  df %>% 
    summarize_if(is_scaled, funs(get_scale)) %>% 
    gather(term, scaled)
}

# H1 ---------------------------------------------------------------------
h1.1 <- read_rds(here("Data", "data_cache", "h1_1.rds.gz")) %>% 
  mutate(scales_h1.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))
  
h1.2 <- read_rds(here("Data", "data_cache", "h1_2.rds.gz")) %>% 
  mutate(scales_h1.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h1.3 <- read_rds(here("Data", "data_cache", "h1_3.rds.gz")) %>% 
  mutate(scales_h1.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

# Models
mods.h1.next_year.raw.bayes <- h1.1 %>%
  left_join(h1.2, by = "m") %>%
  left_join(h1.3, by = "m")

write_rds(mods.h1.next_year.raw.bayes,
          here("Data", "data_cache", "models_bayes_h1.rds"),
          compress = "gz")


# H2 ---------------------------------------------------------------------
h2.1 <- readRDS(here("Data", "data_cache", "h2_1.rds.gz")) %>% 
  mutate(scales_h2.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h2.2 <- readRDS(here("Data", "data_cache", "h2_2.rds.gz")) %>% 
  mutate(scales_h2.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h2.3 <- readRDS(here("Data", "data_cache", "h2_3.rds.gz")) %>% 
  mutate(scales_h2.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h2.next_year.raw.bayes <- h2.1 %>%
  left_join(h2.2, by = "m") %>%
  left_join(h2.3, by = "m")

write_rds(mods.h2.next_year.raw.bayes,
          here("Data", "data_cache", "models_bayes_h2.rds"),
          compress = "gz")


# H2 zoib -----------------------------------------------------------------
h2.1.zoib <- readRDS(here("Data", "data_cache", "h2_1_zoib.rds.gz")) %>% 
  mutate(scales_h2.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h2.2.zoib <- readRDS(here("Data", "data_cache", "h2_2_zoib.rds.gz")) %>% 
  mutate(scales_h2.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h2.3.zoib <- readRDS(here("Data", "data_cache", "h2_3_zoib.rds.gz")) %>% 
  mutate(scales_h2.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h2.next_year.raw.bayes.zoib <- h2.1.zoib %>%
  left_join(h2.2.zoib, by = "m") %>%
  left_join(h2.3.zoib, by = "m")

write_rds(mods.h2.next_year.raw.bayes.zoib,
          here("Data", "data_cache", "models_bayes_h2_zoib.rds"),
          compress = "gz")


# H3 domestic ------------------------------------------------------------
h3_dom.1 <- readRDS(here("Data", "data_cache", "h3_dom_1.rds.gz")) %>% 
  mutate(scales_h3_dom.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_dom.2 <- readRDS(here("Data", "data_cache", "h3_dom_2.rds.gz")) %>%
  mutate(scales_h3_dom.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_dom.3 <- readRDS(here("Data", "data_cache", "h3_dom_3.rds.gz")) %>% 
  mutate(scales_h3_dom.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h3.dom.next_year.bayes <- h3_dom.1 %>%
  left_join(h3_dom.2, by = "m") %>%
  left_join(h3_dom.3, by = "m")

write_rds(mods.h3.dom.next_year.bayes,
          here("Data", "data_cache", "models_bayes_h3_domestic.rds"),
          compress = "gz")


# H3 domestic zoib --------------------------------------------------------
h3_dom.1.zoib <- readRDS(here("Data", "data_cache", "h3_dom_1_zoib.rds.gz")) %>% 
  mutate(scales_h3_dom.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_dom.2.zoib <- readRDS(here("Data", "data_cache", "h3_dom_2_zoib.rds.gz")) %>% 
  mutate(scales_h3_dom.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_dom.3.zoib <- readRDS(here("Data", "data_cache", "h3_dom_3_zoib.rds.gz")) %>% 
  mutate(scales_h3_dom.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h3.dom.next_year.bayes.zoib <- h3_dom.1.zoib %>%
  left_join(h3_dom.2.zoib, by = "m") %>%
  left_join(h3_dom.3.zoib, by = "m")

write_rds(mods.h3.dom.next_year.bayes.zoib,
          here("Data", "data_cache", "models_bayes_h3_domestic_zoib.rds"),
          compress = "gz")


# H3 foreign -------------------------------------------------------------
h3_foreign.1 <- readRDS(here("Data", "data_cache", "h3_foreign_1.rds.gz")) %>% 
  mutate(scales_h3_foreign.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_foreign.2 <- readRDS(here("Data", "data_cache", "h3_foreign_2.rds.gz")) %>% 
  mutate(scales_h3_foreign.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_foreign.3 <- readRDS(here("Data", "data_cache", "h3_foreign_3.rds.gz")) %>% 
  mutate(scales_h3_foreign.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h3.foreign.next_year.bayes <- h3_foreign.1 %>%
  left_join(h3_foreign.2, by = "m") %>%
  left_join(h3_foreign.3, by = "m")

write_rds(mods.h3.foreign.next_year.bayes,
          here("Data", "data_cache", "models_bayes_h3_foreign.rds"),
          compress = "gz")


# H3 foreign zoib ---------------------------------------------------------
h3_foreign.1.zoib <- readRDS(here("Data", "data_cache", "h3_foreign_1_zoib.rds.gz")) %>% 
  mutate(scales_h3_foreign.1 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_foreign.2.zoib <- readRDS(here("Data", "data_cache", "h3_foreign_2_zoib.rds.gz")) %>% 
  mutate(scales_h3_foreign.2 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

h3_foreign.3.zoib <- readRDS(here("Data", "data_cache", "h3_foreign_3_zoib.rds.gz")) %>% 
  mutate(scales_h3_foreign.3 = data_scaled %>% map(get_scales)) %>% 
  select(-starts_with("data"))

mods.h3.foreign.next_year.bayes.zoib <- h3_foreign.1.zoib %>%
  left_join(h3_foreign.2.zoib, by = "m") %>%
  left_join(h3_foreign.3.zoib, by = "m")

write_rds(mods.h3.foreign.next_year.bayes.zoib,
          here("Data", "data_cache", "models_bayes_h3_foreign_zoib.rds"),
          compress = "gz")
