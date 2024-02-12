# stefan gehrig, 2024-02-12
# last update of folder 'FSP_Exchange' from sharepoint: 2024-12-02, 12:30

##############################
#### import and configure ####
##############################
# libraries
library(tidyverse)
library(pROC)

# import analysis excel sheets
oview <- as_tibble(openxlsx::read.xlsx("FSP_Exchange/data/Overview_Stat_Analyses.xlsx", 
                                       sheet = "Statistics", 
                                       rows = 2:2e2,
                                       detectDates = TRUE))
prevs <- as_tibble(openxlsx::read.xlsx("FSP_Exchange/data/Overview_Stat_Analyses.xlsx", 
                                       sheet = "Prevalence"))

# reshape to have end points in long format
oview %>% 
  pivot_longer(cols = contains("EP", ignore.case = FALSE),
               names_to = "var", values_to = "val") %>% View

# select analyses to run
oview$Analysis_ID
ids_to_compute <- c("5_FMF_UK_A1", "5_FMF_UK_A2")

# merge with relevant data sets
oview %>% 
  filter(Analysis_ID %in% ids_to_compute) %>% 
  select(
    Analysis_ID
  )

# merge with realized outcomes


# merge with prevalences

######################################
#### run performance computations ####
######################################



##############################
#### run validation tests ####
##############################
