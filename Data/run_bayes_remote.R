# devtools::install_github("robertzk/s3mpi@0.2.31")
# nohup Rscript run_bayes_remote.R > bayes.log 2>&1 &
print("Loading data")
source(file.path(PROJHOME, "Analysis", "models-chunks.R"))

source(file.path(PROJHOME, "Analysis", "h1_model_definitions_bayes.R"))
source(file.path(PROJHOME, "Analysis", "h2_model_definitions_bayes.R"))
source(file.path(PROJHOME, "Analysis", "h3_model_definitions_bayes.R"))

raw.dir <- file.path(PROJHOME, "Data", "data_cache")

# H1 ---------------------------------------------------------------------
print("Running H1 models")
h1.raw.path <- file.path(PROJHOME, "Data", "data_cache", "models_bayes_h1.rds")

if (file.exists(h1.raw.path)) {
  mods.h1.next_year.raw.bayes <- readRDS(h1.raw.path)
} else {
  mods.h1.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
    select(one_of(c("m", "total.oda_log_next_year", h1.ivs))) %>%
    nest(-m) 
  
  h1.barriers.total <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.barriers.total = data %>% map(mod.h1.barriers.total.bayes,
                                                "total.oda_log_next_year"))
  saveRDS(h1.barriers.total, file.path(raw.dir, "h1_1.rds"))
  
  h1.barriers.new <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.barriers.new = data %>% map(mod.h1.barriers.new.bayes,
                                              "total.oda_log_next_year"))
  saveRDS(h1.barriers.new, file.path(raw.dir, "h1_2.rds"))
  
  h1.type.total <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.type.total = data %>% map(mod.h1.type.total.bayes,
                                            "total.oda_log_next_year"))
  saveRDS(h1.type.total, file.path(raw.dir, "h1_3.rds"))
  
  h1.type.new <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.type.new = data %>% map(mod.h1.type.new.bayes,
                                          "total.oda_log_next_year"))
  saveRDS(h1.type.new, file.path(raw.dir, "h1_4.rds"))
  
  h1.csre <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.csre = data %>% map(mod.h1.csre.bayes,
                                      "total.oda_log_next_year"))
  saveRDS(h1.csre, file.path(raw.dir, "h1_5.rds"))
  
  saveRDS(mods.h1.next_year.raw.bayes.nested, h1.raw.path)
}


# H2 ---------------------------------------------------------------------
print("Running H2 models")
h2.raw.path <- file.path(PROJHOME, "Data", "data_cache", "models_bayes_h2.rds")

if (file.exists(h2.raw.path)) {
  mods.h2.next_year.raw.bayes <- readRDS(h2.raw.path)
} else {
  mods.h2.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
    select(one_of(c("m", "prop.contentious_logit_next_year", h2.ivs))) %>%
    nest(-m) 
  
  h2.barriers.total <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.barriers.total = data %>% map(mod.h2.barriers.total.bayes,
                                                "prop.contentious_logit_next_year"))
  saveRDS(h2.barriers.total, file.path(raw.dir, "h2_1.rds"))
  
  h2.barriers.new <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.barriers.new = data %>% map(mod.h2.barriers.new.bayes,
                                              "prop.contentious_logit_next_year"))
  saveRDS(h2.barriers.new, file.path(raw.dir, "h2_2.rds"))
  
  h2.type.total <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.type.total = data %>% map(mod.h2.type.total.bayes,
                                            "prop.contentious_logit_next_year"))
  saveRDS(h2.type.total, file.path(raw.dir, "h2_3.rds"))
  
  h2.type.new <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.type.new = data %>% map(mod.h2.type.new.bayes,
                                          "prop.contentious_logit_next_year"))
  saveRDS(h2.type.new, file.path(raw.dir, "h2_4.rds"))
  
  h2.csre <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.csre = data %>% map(mod.h2.csre.bayes,
                                      "prop.contentious_logit_next_year"))
  saveRDS(h2.csre, file.path(raw.dir, "h2_5.rds"))
  
  saveRDS(mods.h2.next_year.raw.bayes.nested, h2.raw.path)
}


# H3 ---------------------------------------------------------------------
print("Running H3 domestic models")
h3.domestic.raw.path <- file.path(PROJHOME, "Data", "data_cache",
                                  "models_bayes_h3_domestic.rds")

if (file.exists(h3.domestic.raw.path)) {
  mods.h3.dom.next_year.raw <- readRDS(h3.domestic.raw.path)
} else {
  mods.h3.dom.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
    select(one_of(c("m", "prop.ngo.dom_logit_next_year", h3.ivs))) %>%
    nest(-m) 
  
  h3.barriers.total <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.barriers.total = data %>% map(mod.h3.barriers.total.bayes,
                                                "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.barriers.total, file.path(raw.dir, "h3_dom_1.rds"))
  
  h3.barriers.new <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.barriers.new = data %>% map(mod.h3.barriers.new.bayes,
                                              "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.barriers.new, file.path(raw.dir, "h3_dom_2.rds"))
  
  h3.type.total <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.type.total = data %>% map(mod.h3.type.total.bayes,
                                            "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.type.total, file.path(raw.dir, "h3_dom_3.rds"))
  
  h3.type.new <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.type.new = data %>% map(mod.h3.type.new.bayes,
                                          "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.type.new, file.path(raw.dir, "h3_dom_4.rds"))
  
  h3.csre <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.csre = data %>% map(mod.h3.csre.bayes,
                                      "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.csre, file.path(raw.dir, "h3_dom_5.rds"))
  
  saveRDS(mods.h3.dom.next_year.raw.nested, h3.domestic.raw.path)
}

print("Running H3 foreign models")
h3.foreign.raw.path <- file.path(PROJHOME, "Data", "data_cache",
                                 "models_bayes_h3_foreign.rds")

if (file.exists(h3.foreign.raw.path)) {
  mods.h3.foreign.next_year.raw <- readRDS(h3.foreign.raw.path)
} else {
  mods.h3.foreign.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
    select(one_of(c("m", "prop.ngo.foreign_logit_next_year", h3.foreign.ivs))) %>%
    nest(-m) 
  
  h3.foreign.barriers.total <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.barriers.total = data %>% map(mod.h3.foreign.barriers.total.bayes,
                                                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.barriers.total, file.path(raw.dir, "h3_foreign_1.rds"))
  
  h3.foreign.barriers.new <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.barriers.new = data %>% map(mod.h3.foreign.barriers.new.bayes,
                                                      "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.barriers.new, file.path(raw.dir, "h3_foreign_2.rds"))
  
  h3.foreign.type.total <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.type.total = data %>% map(mod.h3.foreign.type.total.bayes,
                                                    "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.type.total, file.path(raw.dir, "h3_foreign_3.rds"))
  
  h3.foreign.type.new <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.type.new = data %>% map(mod.h3.foreign.type.new.bayes,
                                                  "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.type.new, file.path(raw.dir, "h3_foreign_4.rds"))
  
  h3.foreign.csre <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.csre = data %>% map(mod.h3.foreign.csre.bayes,
                                              "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.csre, file.path(raw.dir, "h3_foreign_5.rds"))
  
  saveRDS(mods.h3.foreign.next_year.raw.nested, h3.foreign.raw.path)
}
