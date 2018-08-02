# On my 2016 quad-core MacBook Pro with 16 GB of RAM, using all four cores with
# plan(multiprocess), each future_map() command below takes ≈45 minutes.
# 
# I used a cluster of 6 DigitalOcean droplets, each with 4 CPUs and 8 GB of RAM
# ("s-4vcpu-8gb" in the API), and each future_map() command took ≈3 minutes,
# which is phenomenal!
#
# NB: Only use as many machines as you need in the cluster. Here, we run models
# on the original data + each of the imputations, so there are 6 models running
# in parallel with future_map(). Using 1-6 machines is fine—future bounces jobs
# across the different machines happily, and 6 gives you the maximum speed.
# Going beyond 6, though, slows things down substantially (like, models take
# twice as long (or more) to run). idk why, but too big of a cluster is bad.
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

# Note: 
library(analogsea)
library(furrr)
library(tictoc)

# Path to private SSH key that matches key on DigitalOcean
ssh_private_key_file <- "/Users/andrew/.ssh/id_rsa"

# Droplet settings
droplet_region <- "sfo2"
droplet_size <- "s-4vcpu-8gb"

# Create a new droplet with Docker pre-installed
droplet1 <- docklet_create(region = droplet_region, size = droplet_size)

# Pull the docker image with the environment for this project
# NB: Wait for a couple minutes before running this
droplet(droplet1$id) %>% 
  droplet_wait() %>% 
  docklet_pull("andrewheiss/docker-donors-ngo-restrictions")

droplet(droplet1$id) %>% 
  droplet_power_off() %>% 
  droplet_snapshot(name = "docker_ready") %>% 
  droplet_power_on()

snapshot_id <- images(private = TRUE)$docker_ready$id

# -----------------------------------------
# ↓ Repeat this as many times as needed ↓
# -----------------------------------------
# But only a maximum of 6, really
droplet2 <- droplet_create(image = snapshot_id, region = droplet_region, size = droplet_size)
droplet3 <- droplet_create(image = snapshot_id, region = droplet_region, size = droplet_size)
droplet4 <- droplet_create(image = snapshot_id, region = droplet_region, size = droplet_size)
droplet5 <- droplet_create(image = snapshot_id, region = droplet_region, size = droplet_size)
droplet6 <- droplet_create(image = snapshot_id, region = droplet_region, size = droplet_size)
# -----------------------------------------
# ↑ Repeat this as many times as needed ↑
# -----------------------------------------

# Get IP addresses
remote_droplets <- list(droplet1, droplet2, droplet3, 
                        droplet4, droplet5, droplet6)

ips <- remote_droplets %>% map_chr(~ .$networks$v4[[1]]$ip_address)


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

tic("all_h1")
mods.h1.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
  select(one_of(c("m", "total.oda_log_next_year", h1.ivs))) %>%
  nest(-m) 

tic()
h1.barriers.total <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.barriers.total = data %>% 
           future_map(mod.h1.barriers.total.bayes, 
                      "total.oda_log_next_year"))
saveRDS(h1.barriers.total, file.path(raw.dir, "h1_1.rds"))
toc()

tic()
h1.barriers.new <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.barriers.new = data %>% 
           future_map(mod.h1.barriers.new.bayes, 
                      "total.oda_log_next_year"))
saveRDS(h1.barriers.new, file.path(raw.dir, "h1_2.rds"))
toc()

tic()
h1.type.total <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.type.total = data %>% 
           future_map(mod.h1.type.total.bayes, 
                      "total.oda_log_next_year"))
saveRDS(h1.type.total, file.path(raw.dir, "h1_3.rds"))
toc()

tic()
h1.type.new <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.type.new = data %>% 
           future_map(mod.h1.type.new.bayes, 
                      "total.oda_log_next_year"))
saveRDS(h1.type.new, file.path(raw.dir, "h1_4.rds"))
toc()

tic()
h1.csre <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.csre = data %>% 
           future_map(mod.h1.csre.bayes, 
                      "total.oda_log_next_year"))
saveRDS(h1.csre, file.path(raw.dir, "h1_5.rds"))
toc()

toc()


# H2 ---------------------------------------------------------------------
print("Running H2 models")

tic("all_h2")
mods.h2.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
  select(one_of(c("m", "prop.contentious_logit_next_year", h2.ivs))) %>%
  nest(-m) 

tic()
h2.barriers.total <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.barriers.total = data %>% 
           future_map(mod.h2.barriers.total.bayes, 
                      "prop.contentious_logit_next_year"))
saveRDS(h2.barriers.total, file.path(raw.dir, "h2_1.rds"))
toc()

tic()
h2.barriers.new <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.barriers.new = data %>% 
           future_map(mod.h2.barriers.new.bayes,
                      "prop.contentious_logit_next_year"))
saveRDS(h2.barriers.new, file.path(raw.dir, "h2_2.rds"))
toc()

tic()
h2.type.total <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.type.total = data %>% 
           future_map(mod.h2.type.total.bayes,
                      "prop.contentious_logit_next_year"))
saveRDS(h2.type.total, file.path(raw.dir, "h2_3.rds"))
toc()

tic()
h2.type.new <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.type.new = data %>% 
           future_map(mod.h2.type.new.bayes,
                      "prop.contentious_logit_next_year"))
saveRDS(h2.type.new, file.path(raw.dir, "h2_4.rds"))
toc()

tic()
h2.csre <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.csre = data %>% 
           future_map(mod.h2.csre.bayes,
                      "prop.contentious_logit_next_year"))
saveRDS(h2.csre, file.path(raw.dir, "h2_5.rds"))
toc()

toc()


# H3 ---------------------------------------------------------------------
print("Running H3 domestic models")

tic("all_h3_domestic")
mods.h3.dom.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", "prop.ngo.dom_logit_next_year", h3.ivs))) %>%
  nest(-m) 

tic()
h3.barriers.total <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.barriers.total = data %>% 
           future_map(mod.h3.barriers.total.bayes,
                      "prop.ngo.dom_logit_next_year"))
saveRDS(h3.barriers.total, file.path(raw.dir, "h3_dom_1.rds"))
toc()

tic()
h3.barriers.new <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.barriers.new = data %>% 
           future_map(mod.h3.barriers.new.bayes,
                      "prop.ngo.dom_logit_next_year"))
saveRDS(h3.barriers.new, file.path(raw.dir, "h3_dom_2.rds"))
toc()

tic()
h3.type.total <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.type.total = data %>% 
           future_map(mod.h3.type.total.bayes,
                      "prop.ngo.dom_logit_next_year"))
saveRDS(h3.type.total, file.path(raw.dir, "h3_dom_3.rds"))
toc()

tic()
h3.type.new <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.type.new = data %>% 
           future_map(mod.h3.type.new.bayes,
                      "prop.ngo.dom_logit_next_year"))
saveRDS(h3.type.new, file.path(raw.dir, "h3_dom_4.rds"))
toc()

tic()
h3.csre <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.csre = data %>% 
           future_map(mod.h3.csre.bayes,
                      "prop.ngo.dom_logit_next_year"))
saveRDS(h3.csre, file.path(raw.dir, "h3_dom_5.rds"))
toc()

toc()

print("Running H3 foreign models")
h3.foreign.raw.path <- here("Data", "data_cache",
                            "models_bayes_h3_foreign.rds")

tic("all_h3_foreign")
mods.h3.foreign.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", "prop.ngo.foreign_logit_next_year", h3.foreign.ivs))) %>%
  nest(-m) 

tic()
h3.foreign.barriers.total <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.barriers.total = data %>% 
           future_map(mod.h3.foreign.barriers.total.bayes,
                      "prop.ngo.foreign_logit_next_year"))
saveRDS(h3.foreign.barriers.total, file.path(raw.dir, "h3_foreign_1.rds"))
toc()

tic()
h3.foreign.barriers.new <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.barriers.new = data %>% 
           future_map(mod.h3.foreign.barriers.new.bayes,
                      "prop.ngo.foreign_logit_next_year"))
saveRDS(h3.foreign.barriers.new, file.path(raw.dir, "h3_foreign_2.rds"))
toc()

tic()
h3.foreign.type.total <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.type.total = data %>% 
           future_map(mod.h3.foreign.type.total.bayes,
                      "prop.ngo.foreign_logit_next_year"))
saveRDS(h3.foreign.type.total, file.path(raw.dir, "h3_foreign_3.rds"))
toc()

tic()
h3.foreign.type.new <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.type.new = data %>% 
           future_map(mod.h3.foreign.type.new.bayes,
                      "prop.ngo.foreign_logit_next_year"))
saveRDS(h3.foreign.type.new, file.path(raw.dir, "h3_foreign_4.rds"))
toc()

tic()
h3.foreign.csre <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.csre = data %>% 
           future_map(mod.h3.foreign.csre.bayes,
                      "prop.ngo.foreign_logit_next_year"))
saveRDS(h3.foreign.csre, file.path(raw.dir, "h3_foreign_5.rds"))
toc()

toc()
