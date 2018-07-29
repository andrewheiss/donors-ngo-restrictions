# Each model was run indiviudally in run_bayes_remote.R because it takes so long. 
# Here we reassemble all the pieces.
library(tidyverse)
library(here)

# H1 ---------------------------------------------------------------------
h1.1 <- readRDS(here("Data", "data_cache", "h1_1.rds")) %>% 
  select(-data)

h1.2 <- readRDS(here("Data", "data_cache", "h1_2.rds")) %>% 
  select(-data)

h1.3 <- readRDS(here("Data", "data_cache", "h1_3.rds")) %>% 
  select(-data)

h1.4 <- readRDS(here("Data", "data_cache", "h1_4.rds")) %>% 
  select(-data)

h1.5 <- readRDS(here("Data", "data_cache", "h1_5.rds")) %>% 
  select(-data)

mods.h1.next_year.raw.bayes <- h1.1 %>%
  left_join(h1.2, by = "m") %>%
  left_join(h1.3, by = "m") %>%
  left_join(h1.4, by = "m") %>%
  left_join(h1.5, by = "m")

saveRDS(mods.h1.next_year.raw.bayes,
        here("Data", "data_cache", "models_bayes_h1.rds"))


# H2 ---------------------------------------------------------------------
h2.1 <- readRDS(here("Data", "data_cache", "h2_1.rds")) %>% 
  select(-data)

h2.2 <- readRDS(here("Data", "data_cache", "h2_2.rds")) %>% 
  select(-data)

h2.3 <- readRDS(here("Data", "data_cache", "h2_3.rds")) %>% 
  select(-data)

h2.4 <- readRDS(here("Data", "data_cache", "h2_4.rds")) %>% 
  select(-data)

h2.5 <- readRDS(here("Data", "data_cache", "h2_5.rds")) %>% 
  select(-data)

mods.h2.next_year.raw.bayes <- h2.1 %>%
  left_join(h2.2, by = "m") %>%
  left_join(h2.3, by = "m") %>%
  left_join(h2.4, by = "m") %>%
  left_join(h2.5, by = "m")

saveRDS(mods.h2.next_year.raw.bayes,
        here("Data", "data_cache", "models_bayes_h2.rds"))


# H3 domestic ------------------------------------------------------------
h3_dom.1 <- readRDS(here("Data", "data_cache", "h3_dom_1.rds")) %>% 
  select(-data)

h3_dom.2 <- readRDS(here("Data", "data_cache", "h3_dom_2.rds")) %>% 
  select(-data)

h3_dom.3 <- readRDS(here("Data", "data_cache", "h3_dom_3.rds")) %>% 
  select(-data)

h3_dom.4 <- readRDS(here("Data", "data_cache", "h3_dom_4.rds")) %>% 
  select(-data)

h3_dom.5 <- readRDS(here("Data", "data_cache", "h3_dom_5.rds")) %>% 
  select(-data)

mods.h3.dom.next_year.bayes <- h3_dom.1 %>%
  left_join(h3_dom.2, by = "m") %>%
  left_join(h3_dom.3, by = "m") %>%
  left_join(h3_dom.4, by = "m") %>%
  left_join(h3_dom.5, by = "m")

saveRDS(mods.h3.dom.next_year.bayes,
        here("Data", "data_cache", "models_bayes_h3_domestic.rds"))


# H3 foreign -------------------------------------------------------------
h3_foreign.1 <- readRDS(here("Data", "data_cache", "h3_foreign_1.rds")) %>% 
  select(-data)

h3_foreign.2 <- readRDS(here("Data", "data_cache", "h3_foreign_2.rds")) %>% 
  select(-data)

h3_foreign.3 <- readRDS(here("Data", "data_cache", "h3_foreign_3.rds")) %>% 
  select(-data)

h3_foreign.4 <- readRDS(here("Data", "data_cache", "h3_foreign_4.rds")) %>% 
  select(-data)

h3_foreign.5 <- readRDS(here("Data", "data_cache", "h3_foreign_5.rds")) %>% 
  select(-data)

mods.h3.foreign.next_year.bayes <- h3_foreign.1 %>%
  left_join(h3_foreign.2, by = "m") %>%
  left_join(h3_foreign.3, by = "m") %>%
  left_join(h3_foreign.4, by = "m") %>%
  left_join(h3_foreign.5, by = "m")

saveRDS(mods.h3.foreign.next_year.bayes,
        here("Data", "data_cache", "models_bayes_h3_foreign.rds"))
