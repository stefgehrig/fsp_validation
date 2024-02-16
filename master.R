# stefan gehrig, 2024-02-12
# last update of folder 'FSP_Exchange' from share point: 2024-12-02, 17:00

#######################################
#### import and configure: general ####
#######################################
# libraries
library(tidyverse)
library(pROC)
library(Hmisc)

# load functions
source("functions.R") # my custom functions
source("FSP_Exchange/programs/diagnostic-performance_2024-02-09.R") # (adapted) BRAHMS' custom functions

# import overview table
oview <- import_overview()
oview %>% View
# select analyses to run (allows to run only a subset)
oview$Analysis_ID
ids_to_compute <- c("5_FMF_UK_A1")

#################################
#### import and process data ####
#################################
# import and append data tables as nested tibble
df <- append_data_tables(oview, ids_to_compute)

# build harmonized outcome vectors
df$ep_data_clean <- map2(df$condition, df$ep_data,
                         build_binary_endpoints)

# build harmonized fsp measurement vectors
df$fsp_data_clean <- map2(df$condition, df$fsp_data,
                          build_measurements)

# merge cleaned outcomes with cleaned measurements
df$sampledata <- map2(df$ep_data_clean, df$fsp_data_clean,
                      merge_endpoints_measurements)

######################################
#### run performance computations ####
######################################
# compute all cutoffs on probability scale as numeric vector
df$cutoff_numeric <- map2_dbl(df$cutoff, df$sampledata,
                              compute_numeric_cutoffs)

# compute all prevalences on probability scale as numeric vector
df$prevalence_numeric <- map2_dbl(df$prevalence, df$sampledata,
                                  compute_numeric_prevalences)


##############################
#### run validation tests ####
##############################




