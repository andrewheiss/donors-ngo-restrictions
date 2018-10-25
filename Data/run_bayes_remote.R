# I use a cluster of 6 DigitalOcean droplets, each with 4 CPUs and 8 GB of RAM
# ("s-4vcpu-8gb" in the API). Here's how long each hypothesis generally takes:
# 
# - H1: 619 seconds (10.3 minutes; 210ish seconds per set of models)
#   (aaaaaahhh these zero-one-inflated models take soooooooo long)
# - H2: 1201 seconds (20 minutes; 400ish seconds per set of models)
# - H2 (zoib): 4192 seconds (69.9 minutes; 1400ish seconds per set of models)
# - H3 (domestic): 972 seconds (16.2 minutes; 740ish seconds per set of models)
# - H3 (domestic; zoib): 2210 seconds (37 minutes; 740ish seconds per set of models)
# - H3 (foreign): 851 seconds (14 minutes; 280ish seconds per set of models)
# - H3 (foreign; zoib): 1971 seconds (32.9 minutes; 660ish seconds per set of models)
#
# NB: Only use as many machines as you need in the cluster. Here, we run models
# on the original data + each of the imputations, so there are 6 models running
# in parallel with future_map(). Using 1-6 machines is fine—future bounces jobs
# across the different machines happily, and 6 gives you the maximum speed.
# Going beyond 6, though, slows things down substantially (like, models take
# twice as long (or more) to run). idk why, but too big of a cluster is bad.
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

# Make a snapshot of the already-pulled Docker image so we don't have to keep pulling it
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

ips <- remote_droplets %>% map_chr(~ droplet(.$id)$networks$v4[[1]]$ip_address)

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
  select(one_of(c("m", h1.lhs, h1.rhs))) %>%
  nest(-m) %>% 
  # Rescale all large variables to help with model convergence (and drop
  # incomplete cases first for more accurate scaling in non-imputed data)
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, internal.conflict.past.5, 
                        natural_disaster.occurrence, post.1989),
                     -one_of(h1.lhs)),
                funs(my_scale))
  }))

tic()
h1.barriers.total <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.barriers.total = data_scaled %>% 
           future_map(mod.h1.barriers.total.bayes))
write_rds(h1.barriers.total, file.path(raw.dir, "h1_1.rds.gz"), compress = "gz")
toc()

tic()
h1.type.total <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.type.total = data_scaled %>% 
           future_map(mod.h1.type.total.bayes))
write_rds(h1.type.total, file.path(raw.dir, "h1_2.rds.gz"), compress = "gz")
toc()

tic()
h1.csre <- mods.h1.next_year.raw.bayes.nested %>%
  mutate(mod.h1.csre = data_scaled %>% 
           future_map(mod.h1.csre.bayes))
write_rds(h1.csre, file.path(raw.dir, "h1_3.rds.gz"), compress = "gz")
toc()

toc()


# H2 ---------------------------------------------------------------------
print("Running H2 models")

tic("all_h2")
mods.h2.next_year.raw.bayes.nested <- df.country.aid.demean.next_year.both %>%
  select(one_of(c("m", h2.lhs, h2.rhs))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.contentious"), 
                        internal.conflict.past.5, natural_disaster.occurrence, post.1989),
                     -one_of(h2.lhs)),
                funs(my_scale))
  }))

tic()
h2.barriers.total <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.barriers.total = data_scaled %>% 
           future_map(mod.h2.barriers.total.bayes))
write_rds(h2.barriers.total, file.path(raw.dir, "h2_1.rds.gz"), compress = "gz")
toc()

tic()
h2.type.total <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.type.total = data_scaled %>% 
           future_map(mod.h2.type.total.bayes))
write_rds(h2.type.total, file.path(raw.dir, "h2_2.rds.gz"), compress = "gz")
toc()

tic()
h2.csre <- mods.h2.next_year.raw.bayes.nested %>%
  mutate(mod.h2.csre = data_scaled %>% 
           future_map(mod.h2.csre.bayes))
write_rds(h2.csre, file.path(raw.dir, "h2_3.rds.gz"), compress = "gz")
toc()

toc()

# Zero-one inflated beta models
tic("all_h2_zoib")
mods.h2.next_year.raw.bayes.nested.zoib <- df.country.aid.demean.next_year.both %>%
  select(one_of(c("m", h2.lhs.zoib, h2.rhs.zoib))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.contentious"), 
                        internal.conflict.past.5, natural_disaster.occurrence, post.1989),
                     -one_of(h2.lhs.zoib)),
                funs(my_scale))
  }))

tic()
h2.barriers.total.zoib <- mods.h2.next_year.raw.bayes.nested.zoib %>%
  mutate(mod.h2.barriers.total.zoib = data_scaled %>% 
           future_map(mod.h2.barriers.total.bayes.zoib))
write_rds(h2.barriers.total.zoib, file.path(raw.dir, "h2_1_zoib.rds.gz"), compress = "gz")
toc()

tic()
h2.type.total.zoib <- mods.h2.next_year.raw.bayes.nested.zoib %>%
  mutate(mod.h2.type.total.zoib = data_scaled %>% 
           future_map(mod.h2.type.total.bayes.zoib))
write_rds(h2.type.total.zoib, file.path(raw.dir, "h2_2_zoib.rds.gz"), compress = "gz")
toc()

tic()
h2.csre.zoib <- mods.h2.next_year.raw.bayes.nested.zoib %>%
  mutate(mod.h2.csre.zoib = data_scaled %>% 
           future_map(mod.h2.csre.bayes.zoib))
write_rds(h2.csre.zoib, file.path(raw.dir, "h2_3_zoib.rds.gz"), compress = "gz")
toc()

toc()


# H3 ---------------------------------------------------------------------
print("Running H3 domestic models")

tic("all_h3_domestic")
mods.h3.dom.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", h3.dom.lhs, h3.dom.rhs))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.ngo"), 
                        internal.conflict.past.5, natural_disaster.occurrence),
                     -one_of(h3.dom.lhs)),
                funs(my_scale))
  }))

tic()
h3.dom.barriers.total <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.barriers.total = data_scaled %>% 
           future_map(mod.h3.dom.barriers.total.bayes))
write_rds(h3.dom.barriers.total, file.path(raw.dir, "h3_dom_1.rds.gz"), compress = "gz")
toc()

tic()
h3.dom.type.total <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.type.total = data_scaled %>% 
           future_map(mod.h3.dom.type.total.bayes))
write_rds(h3.dom.type.total, file.path(raw.dir, "h3_dom_2.rds.gz"), compress = "gz")
toc()

tic()
h3.dom.csre <- mods.h3.dom.next_year.raw.nested %>%
  mutate(mod.h3.csre = data_scaled %>% 
           future_map(mod.h3.dom.csre.bayes))
write_rds(h3.dom.csre, file.path(raw.dir, "h3_dom_3.rds.gz"), compress = "gz")
toc()

toc()


tic("all_h3_domestic_zoib")
mods.h3.dom.next_year.raw.nested.zoib <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", h3.dom.lhs.zoib, h3.dom.rhs.zoib))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.ngo"), 
                        internal.conflict.past.5, natural_disaster.occurrence),
                     -one_of(h3.dom.lhs.zoib)),
                funs(my_scale))
  }))

tic()
h3.dom.barriers.total.zoib <- mods.h3.dom.next_year.raw.nested.zoib %>%
  mutate(mod.h3.barriers.total.zoib = data_scaled %>% 
           future_map(mod.h3.dom.barriers.total.bayes.zoib))
write_rds(h3.dom.barriers.total.zoib, file.path(raw.dir, "h3_dom_1_zoib.rds.gz"), compress = "gz")
toc()

tic()
h3.dom.type.total.zoib <- mods.h3.dom.next_year.raw.nested.zoib %>%
  mutate(mod.h3.type.total.zoib = data_scaled %>% 
           future_map(mod.h3.dom.type.total.bayes.zoib))
write_rds(h3.dom.type.total.zoib, file.path(raw.dir, "h3_dom_2_zoib.rds.gz"), compress = "gz")
toc()

tic()
h3.dom.csre.zoib <- mods.h3.dom.next_year.raw.nested.zoib %>%
  mutate(mod.h3.csre.zoib = data_scaled %>% 
           future_map(mod.h3.dom.csre.bayes.zoib))
write_rds(h3.dom.csre.zoib, file.path(raw.dir, "h3_dom_3_zoib.rds.gz"), compress = "gz")
toc()

toc()


print("Running H3 foreign models")

tic("all_h3_foreign")
mods.h3.foreign.next_year.raw.nested <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", h3.foreign.lhs, h3.foreign.rhs))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.ngo"), 
                        internal.conflict.past.5, natural_disaster.occurrence),
                     -one_of(h3.foreign.lhs)),
                funs(my_scale))
  }))

tic()
h3.foreign.barriers.total <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.barriers.total = data_scaled %>% 
           future_map(mod.h3.foreign.barriers.total.bayes))
write_rds(h3.foreign.barriers.total, file.path(raw.dir, "h3_foreign_1.rds.gz"), compress = "gz")
toc()

tic()
h3.foreign.type.total <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.type.total = data_scaled %>% 
           future_map(mod.h3.foreign.type.total.bayes))
write_rds(h3.foreign.type.total, file.path(raw.dir, "h3_foreign_2.rds.gz"), compress = "gz")
toc()

tic()
h3.foreign.csre <- mods.h3.foreign.next_year.raw.nested %>%
  mutate(mod.h3.foreign.csre = data_scaled %>% 
           future_map(mod.h3.foreign.csre.bayes))
write_rds(h3.foreign.csre, file.path(raw.dir, "h3_foreign_3.rds.gz"), compress = "gz")
toc()

toc()


tic("all_h3_foreign_zoib")
mods.h3.foreign.next_year.raw.nested.zoib <- df.country.aid.us.demean.next_year.both %>%
  select(one_of(c("m", h3.foreign.lhs.zoib, h3.foreign.rhs.zoib))) %>%
  nest(-m) %>% 
  mutate(data_scaled = data %>% map(~ {
    .x %>% 
      filter(complete.cases(.)) %>% 
      mutate_at(vars(-c(cowcode, year, starts_with("prop.ngo"), 
                        internal.conflict.past.5, natural_disaster.occurrence),
                     -one_of(h3.foreign.lhs.zoib)),
                funs(my_scale))
  }))

tic()
h3.foreign.barriers.total.zoib <- mods.h3.foreign.next_year.raw.nested.zoib %>%
  mutate(mod.h3.foreign.barriers.total.zoib = data_scaled %>% 
           future_map(mod.h3.foreign.barriers.total.bayes.zoib))
write_rds(h3.foreign.barriers.total.zoib, file.path(raw.dir, "h3_foreign_1_zoib.rds.gz"), compress = "gz")
toc()

tic()
h3.foreign.type.total.zoib <- mods.h3.foreign.next_year.raw.nested.zoib %>%
  mutate(mod.h3.foreign.type.total.zoib = data_scaled %>% 
           future_map(mod.h3.foreign.type.total.bayes.zoib))
write_rds(h3.foreign.type.total.zoib, file.path(raw.dir, "h3_foreign_2_zoib.rds.gz"), compress = "gz")
toc()

tic()
h3.foreign.csre.zoib <- mods.h3.foreign.next_year.raw.nested.zoib %>%
  mutate(mod.h3.foreign.csre.zoib = data_scaled %>% 
           future_map(mod.h3.foreign.csre.bayes.zoib))
write_rds(h3.foreign.csre.zoib, file.path(raw.dir, "h3_foreign_3_zoib.rds.gz"), compress = "gz")
toc()

toc()


# Delete cluster and snapshot
remote_droplets %>% map(~ droplet_delete(droplet(.$id)))
image_delete(snapshot_id)
