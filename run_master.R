# stefan gehrig, 2024-02-17
# script to run a logged version of the R/master.R script

# possible analysis ids:
# FMF_UK_A1
# FMF_UK_A2
# FMF_UK_A3
# FMF_UK_A4
# FMF_UK_A5
# FMF_UK_A6
# FMF_UK_A1a
# FMF_UK_A1b
# FMF_UK_A2
# FMF_UK_MFs
# FMF_UK_A1
# FMF_UK_A2a
# FMF_UK_A2b
# FMF_UK_A3
# FMF_UK_A4a
# FMF_UK_A4b
# FMF_UK_MFs
# FMF_DE_A1
# FMF_DE_A2
# FMF_UK_A1
# FMF_UK_A2
# FMF_UK_A3
# FMF_UK_A4
# FMF_UK_A5
# FMF_UK_A4
# FMF_UK_A5
# FMF_UK_A6
# FMF_UK_A7
# FMF_UK_A8_MC
# FMF_UK_A8_DC
# FMF_UK_A9_MC
# FMF_UK_A9_DC
# FMF_UK_A1
# FMF_UK_A2
# FMF_UK_A3
# FMF_UK_A4
# FMF_UK_A5
# FMF_UK_A6
# FMF_UK_A7_MC
# FMF_UK_A7_DC
# FMF_UK_A8_MC
# FMF_UK_A8_DC
# FMF_UK_A9_MC
# FMF_UK_A9_DC
# FMF_UK_A10_MC
# FMF_UK_A10_DC
# FMF_UK_A11_MC
# FMF_UK_A11_DC
# FMF_UK_A12_MC
# FMF_UK_A12_DC
# FMF_UK_A1
# FMF_UK_A2
# FMF_UK_A3
# FMF_UK_A4
# FMF_UK_A5
# FMF_UK_A6
# FMF_UK_A7
# FMF_UK_A8
# FMF_UK_A9
# FMF_UK_A10
# FMF_UK_A11
# FMF_UK_A12
# FMF_UK_A13
# FMF_UK_A14
# FMF_UK_A15
# FMF_UK_A16
# FMF_UK_A17
# FMF_UK_A18
# FMF_UK_A19
# FMF_UK_A20_MC
# FMF_UK_A20_DC
# FMF_UK_A21_MC
# FMF_UK_A21_DC
# FMF_UK_A22_MC
# FMF_UK_A22_DC
# FMF_UK_A23_MC
# FMF_UK_A23_DC
# FMF_UK_A24_MC
# FMF_UK_A24_DC
# FMF_UK_A25_MC
# FMF_UK_A25_DC
# FMF_UK_A26_MC
# FMF_UK_A26_DC
# FMF_UK_A27_MC
# FMF_UK_A27_DC
# FMF_UK_A28_MC
# FMF_UK_A28_DC
# FMF_UK_A29_MC
# FMF_UK_A29_DC
# FMF_UK_A30_MC
# FMF_UK_A30_DC
# FMF_UK_A31_MC
# FMF_UK_A31_DC
# FMF_UK_A32_MC
# FMF_UK_A32_DC
# FMF_UK_A33_MC
# FMF_UK_A33_DC
# FMF_UK_A34_MC
# FMF_UK_A34_DC
# FMF_UK_A35_MC
# FMF_UK_A35_DC
# FMF_UK_A36_MC
# FMF_UK_A36_DC
# FMF_UK_A37_MC
# FMF_UK_A37_DC

# choose analysis ids to compute
ids_to_compute <- c("6_FMF_UK_A1a", "6_FMF_UK_MFs")

# update results time identifier
output_time <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
rmarkdown::render("R/run_master_log.Rmd", 
                  output_dir = "results",
                  output_file = paste0("results/results_", output_time, ".html"))