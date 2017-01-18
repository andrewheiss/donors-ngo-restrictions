# Note: This file isn't really intended to every be run by itself. It's mostly
# for enabling and configuring the s3mpi library so that objects can be shared
# between local and remote R instances.
library(s3mpi)
options(s3mpi.path = "s3://ath-data-processing/donors-ngos-restrictions")

# Get data from S3
df.donor.country <- s3read("df_donor_country")
df.donor <- s3read("df_donor")
df.country <- s3read("df_country")
df.country.aid <- s3read("df_country_aid")

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

# Save data in S3
s3store(df.donor.country, "df_donor_country")
s3store(df.donor, "df_donor")
s3store(df.country, "df_country")
s3store(df.country.aid, "df_country_aid")
