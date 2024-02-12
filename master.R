# stefan gehrig, 2024-02-12
# last update of folder 'FSP_Exchange' from share point: 2024-12-02, 17:00

#######################################
#### import and configure: general ####
#######################################
# libraries
library(tidyverse)
library(pROC)

# import overview table
oview <- import_overview()

# select analyses to run (allows to run only a subset)
oview$Analysis_ID
ids_to_compute <- c("5_FMF_UK_A6")

#################################################
#### import and configure: specific analyses ####
#################################################
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



##############################
#### run validation tests ####
##############################




