# stefan gehrig, 2024-02-12

#######################################
#### import and configure: general ####
#######################################
# libraries
library(tidyverse)
library(pROC)
library(Hmisc)

# load functions
source("R/functions.R")
source("R/diagnostic-performance_2024-02-09_sg.R") 

# import overview table
oview <- import_overview()

# select analyses to run (allows to run only a subset)
# (if an analysis is dependent on another analysis, i.e., when comparing posterior
# with prior risk performance, all dependent analyses must be selected jointly)
ids_to_compute <- c(
  "1_FMF_DE_A1",
  "1_FMF_DE_A2",
  "2_FMF_UK_A1",
  "2_FMF_UK_A2",
  "2_FMF_UK_A3",
  "2_FMF_UK_A4",
  "2_FMF_UK_A5",
  "2_FMF_UK_A6",
  "2_FMF_UK_A7",
  "2_FMF_UK_A6s",
  "2_FMF_UK_A7s",
  "2_FMF_UK_A8_MC",
  "2_FMF_UK_A8_DC",
  "2_FMF_UK_A9_MC",
  "2_FMF_UK_A9_DC",
  "3_FMF_UK_A1",
  "3_FMF_UK_A2",
  "3_FMF_UK_A3",
  "3_FMF_UK_A4",
  "3_FMF_UK_A5",
  "3_FMF_UK_A6",
  "3_FMF_UK_A7_MC",
  "3_FMF_UK_A7_DC",
  "3_FMF_UK_A8_MC",
  "3_FMF_UK_A8_DC",
  "3_FMF_UK_A9_MC",
  "3_FMF_UK_A9_DC",
  "3_FMF_UK_A10_MC",
  "3_FMF_UK_A10_DC",
  "3_FMF_UK_A11_MC",
  "3_FMF_UK_A11_DC",
  "3_FMF_UK_A12_MC",
  "3_FMF_UK_A12_DC"
)

# should p-value for prior vs adjusted risk comparisons be computed, using mc nemar's test? (also using alpha = 0.025)
# 'FALSE' means that only point estimates are compared to determine validation success
pval_prior_vs_adj <- FALSE

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
                        cat("\npredicted outcome prevalence:", 
                            format(round(mean(sampledata$y_hat, na.rm = TRUE), 5), nsmall = 5), "")
                        return(sampledata)
})

# compute all desired prevalences on probability scale as numeric vector
df$prevalence_numeric <- map2_dbl(df$prevalence, df$sampledata,
                                  compute_numeric_prevalences)

# #-----------
# df
# # remove those with no positiev class
# df <- df %>% filter(Analysis_ID != "2_FMF_UK_A4", 
#                     Analysis_ID != "2_FMF_UK_A9_MC")
#####-------

# compute the seven performance measures
df <- append_performances(df)

##############################
#### run validation tests ####
##############################

# #-----------
# # remove those with no prior row
# df
# df <- df %>% filter(validation != "DR a.risk > DR prior")
# df
# #####-------

# run validation hypothesis tests
df <- append_test_results(df, pval_prior_vs_adj = pval_prior_vs_adj)

########################
#### export results ####
########################
filename_rds  <- paste0("results/results_", output_time, ".rds")
filename_xlsx <- paste0("results/results_", output_time, ".xlsx")

saveRDS(df, file = filename_rds) # including list columns with case-wise data
openxlsx::write.xlsx(df %>% select(!where(is.list)), file = filename_xlsx) # excluding list columns with case-wise data
