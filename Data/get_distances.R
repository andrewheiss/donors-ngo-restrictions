# title: get_distances.R
#
# author:
#  - name: Andrew Heiss
#    affiliation: Duke University
#    email: andrew.heiss@duke.edu
#
# purpose: Calculate the minimum, capital, and centroid distances between all 
#          countries using the cshapes R package.
#
# original-source: I originally wrote this for my dissertation (see 
#                  https://github.com/andrewheiss/Dissertation/blob/master/Data/R/get_distances.R)
#
# notes: It's best to run this on a separate server because it takes a really
#        long time. I created a fast VPS at DigitalOcean, installed Docker, 
#        installed the rocker/tidyverse image, ran this through RStudio in a 
#        browser, downloaded all the .rds files locally, and then killed the VPS.
#
#        Also, I had to install rgeos because of gpclib restrictions

library(cshapes)  # Has to come before dplyr because it uses plyr
library(parallel)

get.distances <- function(year.to.get) {
  year.to.get <- as.Date(paste0(year.to.get, "-01-01"))
  
  dmat.min <- cshapes::distmatrix(year.to.get, type="mindist", useGW=TRUE)
  saveRDS(dmat.min, paste0("min_", year.to.get, ".rds"))
  
  dmat.capital <- cshapes::distmatrix(year.to.get, type="capdist", useGW=TRUE)
  saveRDS(dmat.capital, paste0("capital_", year.to.get, ".rds"))
  
  dmat.centdist <- cshapes::distmatrix(year.to.get, type="centdist", useGW=TRUE)
  saveRDS(dmat.capital, paste0("cent_", year.to.get, ".rds"))
}

# Calculate the number of cores
n.cores <- detectCores()

# Initiate cluster
cl <- makeCluster(n.cores, type="FORK")

# Calculate all three distances for each year
parSapply(cl, 1990:2015, get.distances)

# Kill the cluster
stopCluster(cl)
