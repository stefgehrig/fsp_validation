# load also (adapted) BRAHMS' custom functions
source("diagnostic-performance_2024-02-09_sg.R") 

# import overview table
import_overview <- function(){
  # import analysis overview table
  oview <- as_tibble(openxlsx::read.xlsx("FSP_Exchange/data/Overview_Stat_Analyses.xlsx", 
                                         sheet = "Statistics", 
                                         rows = 2:1e3,
                                         detectDates = TRUE)) %>% 
    # column name cleaning
    janitor::clean_names(case = "none") %>% 
    rename_with(
      .fn = function(x){paste0(str_sub(x, 16, 18), "_prevalence")},
      .cols = contains("Prevalence")
    )
  
  # reshape to have end points in long format
  oview_long <- oview %>%
    mutate(across(contains("_prevalence"), as.character)) %>% 
    pivot_longer(
      cols = contains("EP", ignore.case = FALSE),
      names_to = "var",
      values_to = "val"
    ) %>%
    mutate(var = ifelse(str_detect(var, "_"), var, paste0(var, "_condition"))) %>%
    separate_wider_delim(var, "_", names = c("EP", "stat")) %>%
    pivot_wider(names_from = stat, values_from = val) %>% 
    # remove non-existing endpoints
    filter(condition != "-")
  
  return(oview_long)
}

# import and append data tables as nested tibble
append_data_tables <- function(oview, ids_to_compute){
  
  # merge with relevant data sets
  df <- oview %>% 
    filter(Analysis_ID %in% ids_to_compute) 
  
  stopifnot(nrow(df)>0)
  
  cat("\nanalysis ids:\n", paste0(df$Analysis_ID, "\n"))
  
  # extract type of outcome
  df <- df %>% 
    mutate(
      type = case_when(
        str_detect(Data_Set_ID, "PE_") ~ "PE",
        str_detect(Data_Set_ID, "Triso_") ~ "Trisomie",
        TRUE ~ NA_character_
      )
    )
  
  # check that all types have been recognized
  stopifnot(all(!is.na(df$type)))
  cat("\nendpoint types | endpoint conditions:\n", paste0(df$type, " | ", df$condition, "\n"))
  
  # import required data sets
  df <- df %>%
    mutate(
      Data_Filename = case_when(
        type == "PE" ~ paste0("FSP_Exchange/data/PE/", Data_Filename),
        type == "Trisomie" ~ paste0("---", Data_Filename),
        TRUE ~ NA_character_
      ),
      Outcomes_Filename =
        case_when(
          type == "PE" ~ "FSP_Exchange/data/PE/Merge_ID_outcome_PE.xlsx",
          type == "Trisomie" ~ "---",
          TRUE ~ NA_character_
        ),
      Outcomes_Filename_Sheet =
        case_when(
          type == "PE" & Trimester == 1 ~ "PE 1T",
          type == "PE" & Trimester == 2 ~ "PE 2T",
          type == "PE" & Trimester == 3 ~ "PE 3T",
          type == "Trisomie" ~ "---",
          TRUE ~ NA_character_
        )
    )
  
  cat("\ndata files | outcome files | outcome sheets:\n", paste0(df$Data_Filename, 
                                " | ", 
                                df$Outcomes_Filename, 
                                " | ",
                                df$Outcomes_Filename_Sheet,
                                "\n"))
  
  df <- bind_cols(df,
                  # import fsp measurement data
                  map_dfr(df$Data_Filename, function(file) {
                    tibble(fsp_data = list(as_tibble(
                      openxlsx::read.xlsx(file, sheet = "FSP output")
                    )))
                  }),
                  # import outcome data
                  map2_dfr(df$Outcomes_Filename, 
                           df$Outcomes_Filename_Sheet,
                           function(file, sheet) {
                             tibble(ep_data = list(as_tibble(
                               openxlsx::read.xlsx(file, sheet = sheet)
                             )))
                           })
  )
  
  # only keep outcome data rows that have corresponding fsp 
  # measurement and vice versa
  df <- bind_cols(
    df %>% select(-fsp_data, -ep_data),
    map2_dfr(df$fsp_data, df$ep_data, function(x,y){
      x <- x %>% 
        filter(
          sample_id %in% y$patient_id
        )
      y <- y %>% 
        filter(
          patient_id %in% x$sample_id
        )
      
      tibble(fsp_data = list(x),
             ep_data = list(y))
      
    })
  )
  
  
  # check that there is same number of outcomes and measurements
  cat("\nsample size fsp:\n", paste0(  map_dbl(df$fsp_data, nrow),"\n"))
  cat("\nsample size outc:\n", paste0(  map_dbl(df$ep_data, nrow),"\n"))
  stopifnot(all(map_dbl(df$fsp_data, nrow) == map_dbl(df$ep_data, nrow)))
  
  # add a sample size column
  df <- df %>% mutate(
    sample_size = map_dbl(df$fsp_data, nrow)
  )
  
  return(df)
}


# build harmonized outcome vectors
build_binary_endpoints <- function(ep_condition, ep_data){
  if(!str_detect(ep_condition, "PE")){
    # y <- as.numeric(...) 
    # TODO: for trisomy
  } else{
    max_weeks <- parse_number(ep_condition)
    stopifnot(nchar(max_weeks) == 2L)
    y <- as.numeric(ep_data$Pregnancy.outcome == "PE" & ep_data$out.ga < max_weeks)
  }
  
  cat("\noutcome prevalences | condition:\n", paste0(round(mean(y), 5),
                                        " (", sum(y), " / ", length(y), ") | ",
                                        ep_condition, "\n"))
  
  tibble(
    patient_id = ep_data$patient_id,
    y = y
  )
}

# build harmonized measurement vectors
build_measurements <- function(ep_condition, fsp_data){
  if(!str_detect(ep_condition, "PE")){
    # pr <- as.numeric(...) 
    # TODO: for trisomy
    
  } else{
    
    max_weeks <- parse_number(ep_condition)
    stopifnot(nchar(max_weeks) == 2L)
    column_id <- which(grepl("post_PE", names(fsp_data)) & grepl(max_weeks, names(fsp_data)))
    stopifnot(length(column_id)==1)
    cat("\ncolumn used:", names(fsp_data)[column_id])
    
    
    pr <- fsp_data %>% pull(!!as.symbol(names(fsp_data)[column_id]))
  }
  
  cat("\nmissings:", paste0(sum(is.na(pr))))
  cat("\nmean:", paste0(round(mean(pr, na.rm = TRUE), 5), "\n"))

  
  tibble(
    sample_id = fsp_data$sample_id,
    pr = pr 
  )
}

# merge cleaned outcomes with cleaned measurements
merge_endpoints_measurements <- function(ep_data_clean, fsp_data_clean){
  temp <- tibble(
    ep_data_clean %>% 
      left_join(fsp_data_clean, by = join_by("patient_id" == "sample_id"))
  )
  cat("\nrows of merged table:", paste0(nrow(temp)))
  return(temp)
  
}

# find cutoffs depending on not surpassing a given sample FPR in %
find_cutoff_from_fpr_string_percent <- function(fpr_cutoff_string, sampledata){
  
  fpr_cutoff_numeric <- parse_number(fpr_cutoff_string)/100
  
  df_cutoff <- as_tibble(roc(sampledata, y, pr, ret = "coords", quiet = TRUE)) %>% 
    mutate(fpr = 1-specificity) %>% 
    filter(fpr < fpr_cutoff_numeric) %>% 
    filter(fpr == max(fpr)) %>% 
    select(threshold, fpr)
  
  stopifnot(nrow(df_cutoff) == 1)
  
  return(df_cutoff$threshold)
  
}

# compute all cutoffs on probability scale as numeric vector
compute_numeric_cutoffs <- function(cutoff, sampledata){
  
  if(grepl("FPR", cutoff) & grepl("\\%", cutoff)) {
    # cutoffs depending on not surpassing a given sample FPR in %:
    cutoff_numeric <- find_cutoff_from_fpr_string_percent(cutoff, sampledata)

    # cutoffs specified directly as ratio:
  } else if(grepl(":", cutoff)) {
    cutoff_numeric <- eval(parse(text = str_replace(cutoff, ":", "/")))
    
  } else {
    # not supported cases: "MoM AFP >2.5", "-"
    cutoff_numeric <- NA_real_
  }
  
  stopifnot(cutoff_numeric >= 0 & cutoff_numeric <= 1)
  cat("\ntransformed cutoff:", cutoff, "\u2192", cutoff_numeric)
  return(cutoff_numeric)
}

# compute all prevalences on probability scale as numeric vector
compute_numeric_prevalences <- function(prevalence, sampledata){
  
  # case should not exist; either given or NA for existing endpoints
  stopifnot(prevalence != "-")
  
  if(as.numeric(prevalence)%%1==0) {
    # prevalence is given as reciprocal in form of an integer:
    prevalence_numeric <- 1/as.numeric(prevalence)
    
    # prevalence from sample data is used:
  } else if(is.na(prevalence)) {
    prevalence_numeric <- mean(sampledata$y)
    
  } else {
    # not supported cases
    prevalence_numeric <- NA_real_
  }
  
  stopifnot(prevalence_numeric >= 0 & prevalence_numeric <= 1)
  cat("\ntransformed prevalence:", prevalence, "\u2192", prevalence_numeric)
  return(prevalence_numeric)
}

# compute the seven performance measures
append_performances <- function(data){
  
  stopifnot(
    # do not compute performances if there are already performanes in input data
    all(names(data) != "DR") 
  )
  
  performances <-
    map2_dfr(data$sampledata, data$prevalence_numeric,
             function(sampledata, prevalence_numeric) {
               diagn_perf(
                 sampledata,
                 ref_outcome = "y",
                 ref_pos = 1,
                 test_outcome = "y_hat",
                 test_pos = 1,
                 prevalence = prevalence_numeric
               )
             })

  cat(ncol(performances), "columns added to the data table")
  
  data %>% 
    bind_cols(
      performances
    )
  
}

# run validation hypothesis tests per row of the data table
run_validation_tests <- function(data){
  
  # extract from data table: cell counts that were returned by diagn_perf()
  tp <- data$tp
  fp <- data$fp
  tn <- data$tn
  fn <- data$fn
  
  # extract from data table: validation conditions
  validation <- data$validation
  
  if(grepl("DR >", validation) & grepl("\\%", validation) & !grepl(",", validation)) {
    # a single validation condition, based on exceeding a DR in %
    DR_0 <- parse_number(validation) / 100
    
    exacttest <- binom.test(x = tp, n = tp + fn, p = DR_0, alternative = "greater")
    scoretest <- prop.test(x = tp, n = tp + fn, p = DR_0, alternative = "greater", 
                           # no continuity correction, fully consistent with how 
                           # wilson score intervals are computed in diagn_perf()
                           correct = FALSE)
    
    pvals <- tibble(
      pval_exacttest = exacttest$p.value,
      pval_scoretest = scoretest$p.value
    ) %>% 
      mutate(across(contains("pval"),
                    .fns = list(signif = ~.x < 0.025),
                    .names = "{.col}_{.fn}"))
    
    # no tests required
  } else if(validation == "-"){
   
    pvals <- tibble(
      pval_exacttest = NA_real_,
      pval_scoretest = NA_real_,
      pval_exacttest_signif = NA,
      pval_scoretest_signif = NA
    ) 
    
  }
  #   # a single validation condition, based on comparison to the prior risk
  # else if(validation == "DR a.risk > DR prior") {
  # 
  #   DR
  #   
  #   
  
  # TODO: other possible conditions
  # } else if(...) {
  # 
  # }

  cat("\ncomputed exact p-value:",pvals$pval_exacttest)
  return(pvals)
}


