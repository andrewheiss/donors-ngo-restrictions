library(tidyverse)
library(httr)
library(OECD)

# Download and parse data from the OECD

# Data variables
oecd.dataset <- "TABLE2A"
oecd.start <- 2006
oecd.end <- 2015

oecd.rds <- file.path(PROJHOME, "Data", "data_raw",
                      "OECD", paste0(oecd.dataset, ".rds"))

if (file.exists(oecd.rds)) {
  # The data was cached; read it in
  df.oecd <- readRDS(oecd.rds)
  oecd.structure <- file.path(PROJHOME, "Data", "data_raw", "OECD",
                              paste0(oecd.dataset, "_structure.rds"))
  
  # TODO: Clean up the OECD data, merging it with human readable columns
} else {
  # http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE2A
  oecd.structure <- get_data_structure(oecd.dataset)
  
  oecd.recipients <- oecd.structure$RECIPIENT %>%
    mutate(id = as.integer(id)) %>%
    filter(id < 901)  # Get rid of aggregated categories and non-countries
  
  oecd.donors <- oecd.structure$DONOR %>%
    mutate(id = as.integer(id)) %>%
    filter(id < 2000)  # Get rid of aggregated categories
  
  oecd.part <- oecd.structure$PART %>%
    filter(id == 1)
  
  oecd.aidtype <- oecd.structure$AIDTYPE %>%
    mutate(id = as.integer(id)) %>%
    filter(id == 206)  # Only net ODA
  
  oecd.datatype <- oecd.structure$DATATYPE
  
  oecd.filter <- paste(paste(oecd.recipients$id, collapse="+"),
                       paste(oecd.donors$id, collapse="+"),
                       paste(oecd.part$id, collapse="+"),
                       paste(oecd.aidtype$id, collapse="+"),
                       paste(oecd.datatype$id, collapse="+"),
                       sep=".")
  
  # OECD::get_dataset() technically creates a correct API call, but the 
  # underlying rsdmx::readSDMX() is **incredibly** slow and hard to debug, so
  # I'd rather not keep downloading giant files from the OECD on the fly.
  # Instead, here I generate the API url, download and cache the raw XML file,
  # call rsdmx::readSDMX() manually, and then save and cache the final data
  # frame so this whole convoluted process doesn't have to happen again.
  
  # df.oecd <- get_dataset(dataset=dataset, filter=oecd.filter, 
  #                        pre_formatted=TRUE)
  
  oecd.url <- paste0("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/",
                     oecd.dataset, "/", oecd.filter, 
                     "/all?startTime=", oecd.start, "&endTime=", oecd.end)
  
  oecd.raw.file <- file.path(PROJHOME, "Data", "data_raw", 
                             "tmp", "oecd.xml")
  
  oecd.get <- GET(oecd.url, 
                  write_disk(oecd.raw.file, overwrite=TRUE), 
                  progress())
  
  # This takes sooooooo long
  df.oecd <- as.data.frame(rsdmx::readSDMX(oecd.raw.file, isURL=FALSE))
  
  # Save this all to disk so it never has to happen again ever ever
  write_csv(df.oecd, file.path(PROJHOME, "Data", "data_raw",
                               "OECD", paste0(oecd.dataset, ".csv")))
  saveRDS(df.oecd, file.path(PROJHOME, "Data", "data_raw",
                             "OECD", paste0(oecd.dataset, ".rds")))
  saveRDS(oecd.structure, file.path(PROJHOME, "Data", "data_raw", "OECD",
                                    paste0(oecd.dataset, "_structure.rds")))
}
