# Note: This file isn't really intended to every be run by itself. It's mostly
# for enabling and configuring the s3mpi library so that objects can be shared
# between local and remote R instances.
library(s3mpi)
options(s3mpi.path = "s3://ath-data-processing/donors-ngos-restrictions")

# -----------------------------------------------------------------------------
# --------------------------- STUFF TO RUN REMOTELY ---------------------------
# -----------------------------------------------------------------------------
# Get data
df.country.aid <- s3read("df_country_aid_no_imputation")
df.country.aid.impute <- s3read("df_country_aid_imputation")

# Save locally (on remote)
saveRDS(df.country.aid, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_country_aid_no_imputation.rds"))

saveRDS(df.country.aid.impute, 
        file.path(PROJHOME, "Data", "data_clean",
                  "df_country_aid_imputation.rds"))

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
s3store(df.country.aid, "df_country_aid_no_imputation")
s3store(df.country.aid.impute, "df_country_aid_imputation")

# Load models
mods.h1.next_year.raw.bayes <- s3read("mods.h1.next_year.raw.bayes")
mod.h1.barriers.total.bayes <- s3read("mod.h1.barriers.total.bayes")
mod.h1.barriers.new.bayes <- s3read("mod.h1.barriers.new.bayes")
mod.h1.type.total.bayes <- s3read("mod.h1.type.total.bayes")
mod.h1.type.new.bayes <- s3read("mod.h1.type.new.bayes")
mod.h1.csre.bayes <- s3read("mod.h1.csre.bayes")
