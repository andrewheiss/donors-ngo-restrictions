# Note: This file isn't really intended to every be run by itself. It's mostly
# for enabling and configuring the s3mpi library so that objects can be shared
# between local and remote R instances.
library(s3mpi)
options(s3mpi.path = "s3://ath-data-processing/donors-ngos-restrictions")

# -----------------------------------------------------------------------------
# --------------------------- STUFF TO RUN REMOTELY ---------------------------
# -----------------------------------------------------------------------------
# Get data
df.donor.country <- s3read("df_donor_country")
df.donor <- s3read("df_donor")
df.country <- s3read("df_country")
df.country.aid <- s3read("df_country_aid")

# Save locally (on remote)
saveRDS(df.donor.country, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_donor_country.rds"))
saveRDS(df.donor, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_donor.rds"))
saveRDS(df.country, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_country.rds"))
saveRDS(df.country.aid, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_country_aid.rds"))


# Save models in S3
s3store(mod.h1.barriers.total.bayes)
s3store(mod.h1.barriers.new.bayes)
s3store(mod.h1.type.total.bayes)
s3store(mod.h1.type.new.bayes)
s3store(mod.h1.csre.bayes)


# -----------------------------------------------------------------------------
# --------------------------- STUFF TO RUN LOCALLY ----------------------------
# -----------------------------------------------------------------------------
# Save data
s3store(df.donor.country, "df_donor_country")
s3store(df.donor, "df_donor")
s3store(df.country, "df_country")
s3store(df.country.aid, "df_country_aid")


# Load models
mod.h1.barriers.total.bayes <- s3read("mod.h1.barriers.total.bayes")
mod.h1.barriers.new.bayes <- s3read("mod.h1.barriers.new.bayes")
mod.h1.type.total.bayes <- s3read("mod.h1.type.total.bayes")
mod.h1.type.new.bayes <- s3read("mod.h1.type.new.bayes")
mod.h1.csre.bayes <- s3read("mod.h1.csre.bayes")
