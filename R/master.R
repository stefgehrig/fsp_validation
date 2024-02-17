# stefan gehrig, 2024-02-12

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
# (if an analysis is dependent on another analysis, i.e., when comparing posterior
# with prior risk performance, all dependent analyses must be selected jointly)
# ids_to_compute <- c("6_FMF_UK_A1a", "6_FMF_UK_MFs")

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
                        cat("\npredicted outcome prevalence:", mean(sampledata$y_hat, na.rm = TRUE), "\n")
                        return(sampledata)
})

# compute all desired prevalences on probability scale as numeric vector
df$prevalence_numeric <- map2_dbl(df$prevalence, df$sampledata,
                                  compute_numeric_prevalences)

# compute the seven performance measures
df <- append_performances(df)

##############################
#### run validation tests ####
##############################
# run validation hypothesis tests
df <- append_test_results(df)

########################
#### export results ####
########################
filename_rds  <- paste0("../results/results_", output_time, ".rds")
filename_xlsx <- paste0("../results/results_", output_time, ".xlsx")

saveRDS(df, file = filename_rds) # including list columns with case-wise data
openxlsx::write.xlsx(df %>% select(!where(is.list)), file = filename_xlsx) # excluding list columns with case-wise data
