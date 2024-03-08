# stefan gehrig, 2024-02-17
# script to run a logged version of the R/master.R script

# select analyses to run (allows to run only a subset)
# (if an analysis is dependent on another analysis, i.e., when comparing posterior
# with prior risk performance, all dependent analyses must be selected jointly)
ids_to_compute <- c(
  openxlsx::read.xlsx("FSP_Exchange/data/Overview_Stat_Analyses.xlsx",
                      sheet = "Statistics", 
                      rows = 2:1e3)$Analysis_ID
)

# should p-value for prior vs adjusted risk comparisons be computed, using mc nemar's test? (also using alpha = 0.025)
# 'FALSE' means that only point estimates are compared to determine validation success
pval_prior_vs_adj <- FALSE

# update results time identifier
output_time <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
rmarkdown::render("R/run_master_log.Rmd", 
                  output_dir = "results",
                  output_file = paste0("results/results_", output_time, ".html"))
