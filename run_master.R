# stefan gehrig, 2024-02-17
# script to run a logged version of the R/master.R script

# select analyses to run (allows to run only a subset)
# (if an analysis is dependent on another analysis, i.e., when comparing posterior
# with prior risk performance, all dependent analyses must be selected jointly)
ids_to_compute <- c(
  "5_FMF_UK_A1" , "5_FMF_UK_A2"  , "5_FMF_UK_A3",  "5_FMF_UK_A4" ,"5_FMF_UK_A5"  , "5_FMF_UK_A6",  "6_FMF_UK_A1a", "6_FMF_UK_A1b",
  "6_FMF_UK_A2" , "6_FMF_UK_MFs" , "7_FMF_UK_A1",  "7_FMF_UK_A2a", "7_FMF_UK_A2b", "7_FMF_UK_A3",  "7_FMF_UK_A4a", "7_FMF_UK_A4b",
  "7_FMF_UK_MFs"
)

# update results time identifier
output_time <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
rmarkdown::render("R/run_master_log.Rmd", 
                  output_dir = "results",
                  output_file = paste0("results/results_", output_time, ".html"))
