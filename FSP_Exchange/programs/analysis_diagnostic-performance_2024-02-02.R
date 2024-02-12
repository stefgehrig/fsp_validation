#### header ####
# FITRAPS Study Analysis: 
# computation of diagnostic performance measures for binary risks
#
# Input:
#    + study data with FSP risks
# Output:
#    + table of diagnostic performances written to csv-file
#
# Jan C Wiemer, Thermo Fisher Scientific, 2024-02-02
#       
# windows console commands for batch mode processing:
# cd /d A:\Projects\PNS\FITRAPS\FITRAPS_analysis_2024-01\programs
# "C:\Program Files\R\R-4.2.3\bin\x64\R" CMD BATCH "analysis_diagnostic-performance_2024-02-02.R" "analysis_diagnostic-performance_2024-02-02.Rout"
#---------------------------------------------------------------------------------------------.

#---------------------------------------------------------------------------------------------.
# script initialization ####

# initialize workspace
pkg1 <- names(sessionInfo()$otherPkgs)
if (!identical(pkg1, character(0)) & !is.null(pkg1)) { 
  invisible( lapply(paste( 'package:', pkg1,  sep=""), 
                    function(x) detach(x, character.only=TRUE, unload=TRUE, force=TRUE) ) ) 
}
rm(list=ls())
graphics.off()
try( assign("last.warning", NULL, envir = baseenv()), silent = TRUE)
options(nwarnings = 10000)
set.seed(123)

# set working directory
setwd("A:/Projects/PNS/FITRAPS/FITRAPS_analysis_2024-01/programs")

#---------------------.
# control R-packages,
# install from PosIt Public Package Manager website

# specify settings
install_pkgs      <- FALSE                       # set to TRUE to install packages from CRAN, or to FALSE to use the installed collection of R-packages
my_r_version      <- "R version 4.2.3 (2023-03-15 ucrt)"
my_snapshot_date  <- "2023-04-20"                       # date of R-packages to be fixed (R-4.2.3)
#my_repos_location <- "C:/Users/jan.wiemer1/Documents"  # location of the repository of R-packages on the local computer (or central folder)
#my_repos_location <- "C:/Users/andrej.schwabe/R_packages/PPPM"  # location of the repository of R-packages on the local computer (or central folder)
my_repos_location <- "C:/Users/jan.wiemer1/R_packages/PPPM"
if (my_r_version != version$version.string) { stop( paste( "Wrong R-version! Please use ", my_r_version, ".", sep = "") ) }

if ( install_pkgs ) {
  # R Packages to be installed
  pkgs <- c("")
  
  # install packages if needed
  source("I:/Bereichsaustausch/Biostatistics/Software/StatisticsTools/R/win-library/standard_functions/r-package-version-control_2023-09-12.R")
  my.install.packages(pkgs          = pkgs, 
                      snapshot_date = my_snapshot_date, 
                      r_version     = paste(version$major, version$minor, sep = "."),
                      repos         = my_repos_location)
}

# load packages
.libPaths( paste(my_repos_location, "/R-", version$major, ".", version$minor, "_", my_snapshot_date, sep = "") )
library(tidyverse) 
library(lubridate)
library(readxl)
library(Hmisc)

#---------------------.
# set standard paths
path2data    <- "../data"
path2results <- "../results"
path2stdfcts <- "I:/Bereichsaustausch/Biostatistics/Software/StatisticsTools/R/win-library/standard_functions"

# load standard functions
#source(paste(path2stdfcts,"/assay_comparison_2017-02-15.R",sep=""))
source("diagnostic-performance_2024-02-09.R")

# standard colors
blue1 <- "#2166ac"
red1  <- "#b2182b"

#---------------------------------------------------------------------------------------------.
#### load data ####

# study data, twice pre-processed (PI & us)
load("../data/transfer_2024-01-30/FITRAPS_data-with-risks_2024-01-30.RObj") # d
d1 <- d

# data dictionary
dd_pi <- read_excel("../data/transfer_2024-01-19/data-dictionary_2024-01-19.xlsx", sheet="processed_data")

dd <- read_csv("../data/transfer_2024-01-30/data-dictionary_2024-01-30_explore.csv")


#######################################################################################################################.
# start main part

# contingency tables as starting point
addmargins(table(d$risk_pe_before_w37_cut100_ignore_pappa_h4, pe_preterm=d$pe_preterm, useNA="a"))
addmargins(table(d$risk_pe_before_w37_cut100_ignore_utapi_h5, pe_preterm=d$pe_preterm, useNA="a"))
addmargins(table(d$risk_pe_before_w37_cut100_all_h6, pe_preterm=d$pe_preterm, useNA="a"))

summary(d$risk_pe_before_w37_ignore_pappa_h1)
summary(d$risk_pe_before_w37_ignore_utapi_h2)
summary(d$risk_pe_before_w37_all_h3)

# ---------------------------------------------------------------------------------------------.
# compute prognostic performances ####

vars = c("risk_pe_before_w37_cut100_ignore_pappa_h4", "risk_pe_before_w37_cut100_ignore_utapi_h5", "risk_pe_before_w37_cut100_all_h6",
         "risk_pe_before_w37_cut70_ignore_pappa_sec", "risk_pe_before_w37_cut70_ignore_utapi_sec", "risk_pe_before_w37_cut70_all_sec", 
         "risk_pe_before_w34_cut100_ignore_pappa_h4", "risk_pe_before_w34_cut100_ignore_utapi_h5", "risk_pe_before_w34_cut100_all_h6", 
         "risk_pe_before_w34_cut70_ignore_pappa_sec", "risk_pe_before_w34_cut70_ignore_utapi_sec", "risk_pe_before_w34_cut70_all_sec")

names = c("PE < wk37 (cut-off 1:100, ´ignore PAPP-A´)", "PE < wk37 (cut-off 1:100, ´ignore UtA-PI´)", "PE < wk37 (cut-off 1:100, ´all´)",
          "PE < wk37 (cut-off 1:70, ´ignore PAPP-A´)", "PE < wk37 (cut-off 1:70, ´ignore UtA-PI´)", "PE < wk37 (cut-off 1:70, ´all´)", 
          "PE < wk34 (cut-off 1:100, ´ignore PAPP-A´)", "PE < wk34 (cut-off 1:100, ´ignore UtA-PI´)", "PE < wk34 (cut-off 1:100, ´all´)",
          "PE < wk34 (cut-off 1:70, ´ignore PAPP-A´)", "PE < wk34 (cut-off 1:70, ´ignore UtA-PI´)", "PE < wk34 (cut-off 1:70, ´all´)")

results_perf <- NULL
for(i in 1:length(vars)){
  variable = vars[i]
  name     = names[i]
  outcome  = ifelse( grepl("w37",variable), "pe_preterm", "pe_early_onset")
  #cat( "Variable: ", variable, "\n", sep="" )
  results_perf <- bind_rows( results_perf, tibble( Risk=name, diagn_perf( d, ref_outcome=outcome, ref_pos="Yes", test_outcome=variable, test_pos="high risk", conf_type="Wilson" ) ) )
}

# save all results columns
write_csv(results_perf, file="../results/diagnostic-performance_2024-02-08b.csv")

# save formatted columns only (point estimates with 95%-CIs)
results_perf_extract <- results_perf %>% select( all_of("Risk"), ends_with("_str") )
colnames(results_perf_extract) <- gsub("_str","", colnames(results_perf_extract))
write_csv(results_perf_extract, file="../results/diagnostic-performance-short_2024-02-08b.csv")


# ---------------------------------------------------------------------------------------------.
# check mean calibration ####

mean(d$risk_pe_before_w37_ignore_pappa_h1, na.rm=T)
mean(d$risk_pe_before_w37_ignore_utapi_h2, na.rm=T)
mean(d$risk_pe_before_w37_all_h3, na.rm=T)
mean(d$pe_preterm=="Yes", na.rm=T)
mean(d$risk_pe_before_w37_ignore_pappa_h1, na.rm=T) /mean(d$pe_preterm=="Yes", na.rm=T)
# mean risk is under-estimated by a factor of about 1/2


#---------------------------------------------------------------------------------------------.
# show R session info:
# R version and loaded R packages
sessionInfo()

# display warnings
warnings()