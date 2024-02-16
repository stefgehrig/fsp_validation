# stefan gehrig, 2024-02-12
# last update of folder 'FSP_Exchange' from share point: 2024-02-16, 16:45

#######################################
#### import and configure: general ####
#######################################
# libraries
library(tidyverse)
library(pROC)
library(Hmisc)

# load functions
source("functions.R")

# import overview table
oview <- import_overview()

# select analyses to run (allows to run only a subset)
oview$Analysis_ID
ids_to_compute <- c("5_FMF_UK_A1")

#################################
#### import and process data ####
#################################
# import and append data tables as nested tibble, only keep selected analyses
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

# apply all thresholds and compute discrete predictions
df$sampledata <- map2(df$sampledata, df$cutoff_numeric, 
                      function(sampledata, cutoff_numeric) {
                        sampledata <- sampledata %>% mutate(y_hat = pr > cutoff_numeric)
                        cat("\n predicted outcome prevalence:", mean(sampledata$y_hat))
                        return(sampledata)
})

# compute all prevalences on probability scale as numeric vector
df$prevalence_numeric <- map2_dbl(df$prevalence, df$sampledata,
                                  compute_numeric_prevalences)

# compute the seven performance measures
df <- append_performances(df)

##############################
#### run validation tests ####
##############################
# run validation hypothesis tests per row of the data table
df <- bind_cols(
  df,
  map_dfr(split(df, seq(nrow(df))),
          run_validation_tests)
)
