# stefan gehrig, 2024-02-17
# script to run a logged version of the R/master.R script

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
  "3_FMF_UK_A1a",
  "3_FMF_UK_A1b",
  "3_FMF_UK_A2a",
  "3_FMF_UK_A2b",
  "3_FMF_UK_A3a",
  "3_FMF_UK_A3b",
  "3_FMF_UK_A4a",
  "3_FMF_UK_A4b",
  "3_FMF_UK_A5a",
  "3_FMF_UK_A5b",
  "3_FMF_UK_MFs",
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

# update results time identifier
output_time <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
rmarkdown::render("R/run_master_log.Rmd", 
                  output_dir = "results",
                  output_file = paste0("results/results_", output_time, ".html"))
