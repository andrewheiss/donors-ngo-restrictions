# On my 2016 quad-core MacBook Pro with 16 GB of RAM, using all four cores with
# plan(multiprocess), each future_map() command below takes ≈45 minutes.
# 
# I used a cluster of 6 DigitalOcean droplets, each with 4 CPUs and 8 GB of RAM
# ("s-4vcpu-8gb" in the API), and each future_map() command took ≈3 minutes,
# which is phenomenal!
# 
# Run this as a job in RStudio 1.2+ to have this go in the background
#
# Once this script runs and all the *.rds files are saved, run
# Data/run_bayes_reassemble.R to create final versions of the data

source(here::here("lib", "models-chunks.R"))

source(here("lib", "h1_model_definitions_bayes.R"))
source(here("lib", "h2_model_definitions_bayes.R"))
source(here("lib", "h3_model_definitions_bayes.R"))

raw.dir <- here("Data", "data_cache")


# Set up remote clusters --------------------------------------------------

library(analogsea)
library(furrr)

# Path to private SSH key that matches key on DigitalOcean
ssh_private_key_file <- "/Users/andrew/.ssh/id_rsa"

# -----------------------------------------
# ↓ Repeat this as many times as needed ↓
# -----------------------------------------
# Create a new droplet with Docker pre-installed
droplet1 <- docklet_create(region = "sfo2", size = "2gb")

# Pull the docker image with the environment for this project
# NB: Wait for a couple minutes before running this
droplet(droplet1$id) %>% 
  docklet_pull("andrewheiss/docker-donors-ngo-restrictions")

# Get IP address
ip1 <- droplet1$networks$v4[[1]]$ip_address
# -----------------------------------------
# ↑ Repeat this as many times as needed ↑
# -----------------------------------------

# List of all remote droplets
# ips <- c(ip1, ip2, ip3, ip4, ip5, ip6)
ips <- c(ip1)

# This command is run on each cluster
# The script loads the project image
# --net=host allows it to communicate back to this computer
rscript <- c("sudo", "docker", "run", "--net=host", 
             "andrewheiss/docker-donors-ngo-restrictions", "Rscript")

# Connect and create a cluster
cl <- makeClusterPSOCK(
  ips,
  
  # User name; DO droplets use root by default
  user = "root",
  
  # Use private SSH key registered with DO
  rshopts = c(
    "-o", "StrictHostKeyChecking=no",
    "-o", "IdentitiesOnly=yes",
    "-i", ssh_private_key_file
  ),
  
  rscript = rscript,
  
  # Things to run each time the remote instance starts
  rscript_args = c(
    # Set up .libPaths() for the root user and install future/purrr/furrr packages
    "-e", shQuote("local({p <- Sys.getenv('R_LIBS_USER'); dir.create(p, recursive = TRUE, showWarnings = FALSE); .libPaths(p)})"),
    "-e", shQuote("if (!requireNamespace('furrr', quietly = TRUE)) install.packages('furrr')"),
    # Make sure the remote computer uses all CPU cores with Stan
    "-e", shQuote("options(mc.cores = parallel::detectCores())")
  ),
  
  dryrun = FALSE
)

# Use the cluster of DO droplets as the backend for future and furrr functions
plan(cluster, workers = cl)


# H1 ---------------------------------------------------------------------
print("Running H1 models")
h1.raw.path <- here("Data", "data_cache", "models_bayes_h1.rds")

if (file.exists(h1.raw.path)) {
  mods.h1.next_year.raw.bayes <- readRDS(h1.raw.path)
} else {
  mods.h1.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
    select(one_of(c("m", "total.oda_log_next_year", h1.ivs))) %>%
    nest(-m) 
  
  h1.barriers.total <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.barriers.total = data %>% 
             future_map(mod.h1.barriers.total.bayes, 
                        "total.oda_log_next_year"))
  saveRDS(h1.barriers.total, file.path(raw.dir, "h1_1.rds"))
  
  h1.barriers.new <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.barriers.new = data %>% 
             future_map(mod.h1.barriers.new.bayes, 
                        "total.oda_log_next_year"))
  saveRDS(h1.barriers.new, file.path(raw.dir, "h1_2.rds"))
  
  h1.type.total <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.type.total = data %>% 
             future_map(mod.h1.type.total.bayes, 
                        "total.oda_log_next_year"))
  saveRDS(h1.type.total, file.path(raw.dir, "h1_3.rds"))
  
  h1.type.new <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.type.new = data %>% 
             future_map(mod.h1.type.new.bayes, 
                        "total.oda_log_next_year"))
  saveRDS(h1.type.new, file.path(raw.dir, "h1_4.rds"))
  
  h1.csre <- mods.h1.next_year.raw.bayes.nested %>%
    mutate(mod.h1.csre = data %>% 
             future_map(mod.h1.csre.bayes, 
                        "total.oda_log_next_year"))
  saveRDS(h1.csre, file.path(raw.dir, "h1_5.rds"))
}


# H2 ---------------------------------------------------------------------
print("Running H2 models")
h2.raw.path <- here("Data", "data_cache", "models_bayes_h2.rds")

if (file.exists(h2.raw.path)) {
  mods.h2.next_year.raw.bayes <- readRDS(h2.raw.path)
} else {
  mods.h2.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
    select(one_of(c("m", "prop.contentious_logit_next_year", h2.ivs))) %>%
    nest(-m) 
  
  h2.barriers.total <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.barriers.total = data %>% 
             future_map(mod.h2.barriers.total.bayes, 
                        "prop.contentious_logit_next_year"))
  saveRDS(h2.barriers.total, file.path(raw.dir, "h2_1.rds"))
  
  h2.barriers.new <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.barriers.new = data %>% 
             future_map(mod.h2.barriers.new.bayes,
                        "prop.contentious_logit_next_year"))
  saveRDS(h2.barriers.new, file.path(raw.dir, "h2_2.rds"))
  
  h2.type.total <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.type.total = data %>% 
             future_map(mod.h2.type.total.bayes,
                        "prop.contentious_logit_next_year"))
  saveRDS(h2.type.total, file.path(raw.dir, "h2_3.rds"))
  
  h2.type.new <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.type.new = data %>% 
             future_map(mod.h2.type.new.bayes,
                        "prop.contentious_logit_next_year"))
  saveRDS(h2.type.new, file.path(raw.dir, "h2_4.rds"))
  
  h2.csre <- mods.h2.next_year.raw.bayes.nested %>%
    mutate(mod.h2.csre = data %>% 
             future_map(mod.h2.csre.bayes,
                        "prop.contentious_logit_next_year"))
  saveRDS(h2.csre, file.path(raw.dir, "h2_5.rds"))
}


# H3 ---------------------------------------------------------------------
print("Running H3 domestic models")
h3.domestic.raw.path <- here("Data", "data_cache",
                             "models_bayes_h3_domestic.rds")

if (file.exists(h3.domestic.raw.path)) {
  mods.h3.dom.next_year.raw <- readRDS(h3.domestic.raw.path)
} else {
  mods.h3.dom.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
    select(one_of(c("m", "prop.ngo.dom_logit_next_year", h3.ivs))) %>%
    nest(-m) 
  
  h3.barriers.total <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.barriers.total = data %>% 
             future_map(mod.h3.barriers.total.bayes,
                        "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.barriers.total, file.path(raw.dir, "h3_dom_1.rds"))
  
  h3.barriers.new <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.barriers.new = data %>% 
             future_map(mod.h3.barriers.new.bayes,
                        "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.barriers.new, file.path(raw.dir, "h3_dom_2.rds"))
  
  h3.type.total <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.type.total = data %>% 
             future_map(mod.h3.type.total.bayes,
                        "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.type.total, file.path(raw.dir, "h3_dom_3.rds"))
  
  h3.type.new <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.type.new = data %>% 
             future_map(mod.h3.type.new.bayes,
                        "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.type.new, file.path(raw.dir, "h3_dom_4.rds"))
  
  h3.csre <- mods.h3.dom.next_year.raw.nested %>%
    mutate(mod.h3.csre = data %>% 
             future_map(mod.h3.csre.bayes,
                        "prop.ngo.dom_logit_next_year"))
  saveRDS(h3.csre, file.path(raw.dir, "h3_dom_5.rds"))
}

print("Running H3 foreign models")
h3.foreign.raw.path <- here("Data", "data_cache",
                            "models_bayes_h3_foreign.rds")

if (file.exists(h3.foreign.raw.path)) {
  mods.h3.foreign.next_year.raw <- readRDS(h3.foreign.raw.path)
} else {
  mods.h3.foreign.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
    select(one_of(c("m", "prop.ngo.foreign_logit_next_year", h3.foreign.ivs))) %>%
    nest(-m) 
  
  h3.foreign.barriers.total <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.barriers.total = data %>% 
             future_map(mod.h3.foreign.barriers.total.bayes,
                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.barriers.total, file.path(raw.dir, "h3_foreign_1.rds"))
  
  h3.foreign.barriers.new <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.barriers.new = data %>% 
             future_map(mod.h3.foreign.barriers.new.bayes,
                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.barriers.new, file.path(raw.dir, "h3_foreign_2.rds"))
  
  h3.foreign.type.total <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.type.total = data %>% 
             future_map(mod.h3.foreign.type.total.bayes,
                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.type.total, file.path(raw.dir, "h3_foreign_3.rds"))
  
  h3.foreign.type.new <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.type.new = data %>% 
             future_map(mod.h3.foreign.type.new.bayes,
                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.type.new, file.path(raw.dir, "h3_foreign_4.rds"))
  
  h3.foreign.csre <- mods.h3.foreign.next_year.raw.nested %>%
    mutate(mod.h3.foreign.csre = data %>% 
             future_map(mod.h3.foreign.csre.bayes,
                        "prop.ngo.foreign_logit_next_year"))
  saveRDS(h3.foreign.csre, file.path(raw.dir, "h3_foreign_5.rds"))
}
