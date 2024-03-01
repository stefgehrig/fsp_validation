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
  
  # check early that ntd endpoint is only specified with mom afp > 2.5 cutoff,
  # as this is the only specification which works with the program
  stopifnot(
    all(oview_long$cutoff[oview_long$condition == "NTD"] == "MoM AFP >2.5")
  )
  
  # check early that ntd endpoint is only used with a suitable 2nd trimester algorithm,
  # as this is only algorithm it should be used with (see validation plan)
  stopifnot(
    all(oview_long$Agorithm_ID[oview_long$condition == "NTD"] == "3_FMF_UK")
  )
  
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
        str_detect(Data_Set_ID, "Triso_") ~ "Trisomy",
        TRUE ~ NA_character_
      )
    )
  stopifnot(all(!is.na(df$type)))
  
  # check that all types have been recognized
  stopifnot(all(!is.na(df$type)))
  cat("\nendpoint types | endpoint conditions:\n", paste0(df$type, " | ", df$condition, "\n"))
  
  # import required data sets
  df <- df %>%
    mutate(
      Data_Filename = case_when(
        type == "PE" ~ paste0("FSP_Exchange/data/PE/", Data_Filename),
        type == "Trisomy" ~ paste0("FSP_Exchange/data/Trisomy/", Data_Filename),
        TRUE ~ NA_character_
      ),
      Outcomes_Filename =
        case_when(
          type == "PE" ~ "FSP_Exchange/data/PE/Merge_ID_outcome_PE.xlsx",
          type == "Trisomy" ~ "FSP_Exchange/data/Trisomy/Merge_ID_outcome_Trisomy.xlsx",
          TRUE ~ NA_character_
        ),
      Outcomes_Filename_Sheet =
        case_when(
          type == "PE" & Trimester == 1 ~ "PE 1T",
          type == "PE" & Trimester == 2 ~ "PE 2T",
          type == "PE" & Trimester == 3 ~ "PE 3T",
          type == "Trisomy" ~ Population_Name,
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
                    ) %>% 
                      # there are duplicated rows in some fsp outputs
                      distinct()))
                  }),
                  # import outcome data
                  map2_dfr(df$Outcomes_Filename, 
                           df$Outcomes_Filename_Sheet,
                           function(file, sheet) {
                             tibble(ep_data = list(as_tibble(
                               openxlsx::read.xlsx(file, sheet = sheet)
                             ) %>% 
                               # correct id column name for outcome data due to inconsistent naming scheme
                               rename_with(.fn = ~ "patient_id",
                                           .cols = any_of(c("id", "patient_id"))) %>% 
                               # correct outcome columns where some strings have irregular white space
                               mutate(across(contains("Pregnancy.outcome"),
                                             ~trimws(.x, which = "both")))
                             ))
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
  
  return(df)
}

# build harmonized outcome vectors
build_binary_endpoints <- function(ep_condition, ep_data){
  
  if(ep_condition %in% c("T21", "T18", "T13", "NTD", "T18/13")){

    ep_condition_vector <- unlist(map(str_split(ep_condition, "\\/"), function(outcome){
      ifelse(grepl("^[0-9]", outcome), paste0("T", outcome), outcome)
    })) # need vector c("T18", "T13") for double condition

      if("Pregnancy.outcome" %in% names(ep_data)){ # only one outcome (no twins)
        y <- as.numeric(ep_data$Pregnancy.outcome %in% ep_condition_vector)
        other_endpoint <- ep_data$Pregnancy.outcome != "Con" & !y
      } else{ # two outcomes (use only first twin)
        y <- as.numeric(ep_data$Pregnancy.outcome.Fetus.1 %in% ep_condition_vector)
        other_endpoint <- ep_data$Pregnancy.outcome.Fetus.1 != "Con" & !y
      }


  } else if(str_detect(ep_condition, "PE<")){
    
    max_weeks <- parse_number(ep_condition)
    stopifnot(nchar(max_weeks) == 2L)
    
    y <- as.numeric(ep_data$Pregnancy.outcome == "PE" & ep_data$out.ga < max_weeks)
    other_endpoint <- ep_data$Pregnancy.outcome != "Con" & !y
    
  } else{
    stop()
  }
  
  cat("observed prop | condition | removed b/c other endpoint:", paste0(format(round(mean(y[!other_endpoint]), 3), nsmall = 3),
                                        " (", format(sum(y[!other_endpoint]), width = 4), " / ", 
                                        format(sum(!is.na(y[!other_endpoint])), width = 5), ") | ",
                                        format(ep_condition, width = 8), " | ",
                                        format(sum(other_endpoint), width = 4),
                                        "\n"))
  
  tibble(
    patient_id = ep_data$patient_id[!other_endpoint],
    y = y[!other_endpoint]
  )
}

# build harmonized measurement vectors
build_measurements <- function(ep_condition, fsp_data, analysis_id){
  
  if(ep_condition %in% c("T21", "T18", "T13", "T18/13")){

    ep_condition_merged <- unlist(map(str_split(ep_condition, "\\/"), function(outcome){
      paste(ifelse(grepl("^[0-9]", outcome), paste0("T", outcome), outcome), collapse = "")
    })) # need form "T18T13" for double condition

    # check via analysis id whether this is an analysis of maternal factors only in trisomy
    # (a different column name will then need to be selected)
    if(endsWith(analysis_id, "MFs")){
      column_name <- paste0("background_",  ep_condition_merged)
    } else{
      column_name <- paste0("post_",  ep_condition_merged) 
      # even if there are twins which ...
      # - have two post risks that might differ (2_FMF_UK DC),
      # - have two post risks that are same (2_FMF_UK MC)
      # - have only one risk (3_FMF_UK)
      # ... this uses only first twin
    }

    column_id <- which(names(fsp_data) == column_name)
    stopifnot(length(column_id) == 1)
    pr <- fsp_data %>% pull(!!as.symbol(names(fsp_data)[column_id]))
      
  } else if(ep_condition == "NTD"){

    column_id <- which(names(fsp_data) == "afp.mom")
    stopifnot(length(column_id) == 1)
    pr <- fsp_data %>% pull(!!as.symbol(names(fsp_data)[column_id]))
    
  } else if(str_detect(ep_condition, "PE<")){
    
    max_weeks <- parse_number(ep_condition)
    stopifnot(nchar(max_weeks) == 2L)
    column_id <- which(grepl("post_PE", names(fsp_data)) & grepl(max_weeks, names(fsp_data)))
    stopifnot(length(column_id) == 1)
    pr <- fsp_data %>% pull(!!as.symbol(names(fsp_data)[column_id]))
    
  } else{
    stop()
  }
  
  cat("column | missings | mean: ")
  cat(format(names(fsp_data)[column_id], width = 20), "|", format(sum(is.na(pr)), width = 4), "|",
      paste0(format(round(mean(pr, na.rm = TRUE), 5), nsmall = 5)), "\n")

  tibble(
    sample_id = fsp_data %>% pull(sample_id),
    pr = pr
  )
}

# merge cleaned outcomes with cleaned measurements
merge_endpoints_measurements <- function(ep_data_clean, fsp_data_clean){
  temp <- tibble(
    ep_data_clean %>% 
      left_join(fsp_data_clean, by = join_by("patient_id" == "sample_id"))
  )
  
  cat("rows of merged table:", nrow(temp), "\n")
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
  
  return(max(df_cutoff$threshold)) # use threshold that maximizes DR, if same fpr holds at many thresholds
  
}

# compute all cutoffs on probability scale as numeric vector
compute_numeric_cutoffs <- function(cutoff, sampledata){
  
  if(grepl("FPR", cutoff) & grepl("\\%", cutoff)) {
    # cutoffs depending on not surpassing a given sample FPR in %:
    cutoff_numeric <- find_cutoff_from_fpr_string_percent(cutoff, sampledata)

    # cutoffs specified directly as ratio of digits:
  } else if(grepl(":", cutoff) & !grepl("[A-Za-z]", cutoff)) {
    cutoff_numeric <- eval(parse(text = str_replace(cutoff, ":", "/")))
    
    # cutoffs defined by mom afp > 2.5 for ntd
  } else if(cutoff == "MoM AFP >2.5"){
    cutoff_numeric <- 2.5
    
  } else{
    stop()
  }
  
  cat("transformed cutoff:", format(cutoff, width = 14), "\u2192", 
      format(round(cutoff_numeric, 5), nsmall = 5), "\n")
  return(cutoff_numeric)
}

# compute all desired prevalences on probability scale as numeric vector
compute_numeric_prevalences <- function(prevalence, sampledata){
  
  # case should not exist; either given as string of digits or NA for existing endpoints
  stopifnot(!grepl("\\D", prevalence))
  
  # prevalence from sample data is used:
  if(is.na(prevalence)) {
    prevalence_numeric <- mean(sampledata$y)
    
  } else if(as.numeric(prevalence)%%1==0) {
    # prevalence is given as reciprocal in form of an integer:
    prevalence_numeric <- 1/as.numeric(prevalence)
  } else {
    # not supported cases
    prevalence_numeric <- NA_real_
  }
  
  stopifnot(prevalence_numeric >= 0 & prevalence_numeric <= 1)
  cat("transformed prevalence:", 
      format(prevalence, width = 5), "\u2192", 
      format(round(prevalence_numeric, 5), nsmall = 5), "\n")
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

  cat(ncol(performances), "columns added to the data table\n")
  
  data %>% 
    bind_cols(
      performances
    )
  
}

# run validation hypothesis tests per row of the data table
run_validation_tests <- function(data, data_prior = NULL, pval_prior_vs_adj = NULL){
  
  # extract from data table: cell counts that were returned by diagn_perf()
  tp <- data$tp
  fp <- data$fp
  tn <- data$tn
  fn <- data$fn
  
  # extract from data table: validation conditions
  validation <- data$validation
  
  # extract from data table: case-level data for prior and posterior risks
  if(!is.null(data_prior)){
    stopifnot(!is.null(pval_prior_vs_adj))
    
    sampledata_postr <- data$sampledata[[1]]
    sampledata_prior <- data_prior$sampledata[[1]]
    
    # rename column names for prior risk data table
    names(sampledata_prior)[!grepl("patient_id", names(sampledata_prior))] <- paste0(names(sampledata_prior)[!grepl("_id", names(sampledata_prior))], "_prior")
    
    sampledata_compare <- left_join(sampledata_postr, sampledata_prior, by = "patient_id") 
    stopifnot(nrow(sampledata_compare) == nrow(sampledata_postr))
    sampledata_compare <- sampledata_compare %>% drop_na() # can only use cases which have all data for prior and posterior
    
    stopifnot(all(sampledata_compare$y == sampledata_compare$y_prior))
    
    if(nrow(sampledata_prior) != nrow(sampledata_postr)){
      cat("Data sets for prior and adjusted risks do not have same length in analysis", data$Analysis_ID, "!",
          "\nOnly cases which occur without missings in both data sets are used for validation test (", nrow(sampledata_compare), ")\n")
    }
    
  }
  
  # a single validation condition, based on exceeding a DR in %
  if(grepl("DR >", validation) & grepl("\\%", validation) & !grepl("\\, ", validation)) {
    DR_0 <- parse_number(validation) / 100
    
    exacttest_1 <- binom.test(x = tp, n = tp + fn, p = DR_0, alternative = "greater")
    
    suppressWarnings(# no need for chi-squared approximation warnings
      scoretest_1  <-
        prop.test(
          x = tp,
          n = tp + fn,
          p = DR_0,
          alternative = "greater",
          # no continuity correction, fully consistent with how
          # wilson score intervals are computed in diagn_perf()
          correct = FALSE
        ))
    
    pvals <- tibble(
      exacttest_1 = exacttest_1$p.value,
      scoretest_1 = scoretest_1$p.value,
      exacttest_2 = NA_real_,
      scoretest_2 = NA_real_
    ) %>% 
      mutate(success = exacttest_1 < 0.025)
    
    # no tests required
  } else if(validation == "-"){
   
    pvals <- tibble(
      exacttest_1 = NA_real_,
      scoretest_1 = NA_real_,
      exacttest_2 = NA_real_,
      scoretest_2 = NA_real_
    )
    
    # a single validation condition, based on comparison to the prior risk
  } else if(validation == "DR a.risk > DR prior") {
    
    
    testdata <- sampledata_compare %>% 
      summarise(
        disagree_postr = sum(y_hat == 1 & y_hat_prior == 0 & y == 1),
        disagree_total = sum(y_hat != y_hat_prior & y == 1)
      )
    
    if(isTRUE(pval_prior_vs_adj)){
      # implement an exact and score test version of mcnemar's test
      exacttest_1  <- binom.test(x = testdata$disagree_postr,
                                 n = testdata$disagree_total, 
                                 p = 1/2,
                                 alternative = "greater")
      
      suppressWarnings(
        # no need for chi-squared approximation warnings
        scoretest_1 <- prop.test(
          x = testdata$disagree_postr,
          n = testdata$disagree_total,
          p = 1 / 2,
          alternative = "greater",
          # no continuity correction, fully consistent with how
          # wilson score intervals are computed in diagn_perf()
          correct = FALSE
        )
      )
      
      # check that my mcnemar test is working properly by comparing to built-in two-sided function
      stopifnot(
        round(mcnemar.test(sampledata_compare %>% 
                             filter(y == 1) %>% 
                             {table(.$y_hat, .$y_hat_prior)},
                           correct = FALSE)$p.value / 2, 12) == round(scoretest$p.value, 12)
      )
      
      pvals <- tibble(
        exacttest_1 = exacttest_1$p.value,
        scoretest_1 = scoretest_1$p.value,
        exacttest_2 = NA_real_,
        scoretest_2 = NA_real_
      )  %>% 
        mutate(success = exacttest_1 < 0.025)
      
    } else{
      
      pvals <- tibble(
        exacttest_1 = NA_real_,
        scoretest_1 = NA_real_,
        exacttest_2 = NA_real_,
        scoretest_2 = NA_real_
      ) %>% 
        mutate(
          success = testdata$disagree_postr > (testdata$disagree_total - testdata$disagree_postr)
        )
    }

    # a single validation condition, based on strictly exceeding a required absolute number of detected cases
  } else if(grepl("> *.* cases", validation)){
    
    required_number <- parse_number(validation)
    
    pvals <- tibble(
      exacttest_1 = NA_real_,
      scoretest_1 = NA_real_,
      exacttest_2 = NA_real_,
      scoretest_2 = NA_real_
    ) %>% 
      mutate(
        success = tp > required_number
      )
    
    # a joint condition, based on based on exceeding a DR and a specificity in %
  } else if(grepl("DR >", validation) & grepl("FPR <", validation) & grepl("\\, ", validation) & grepl("\\%", validation)){
    
    DR_0  <- as.numeric(gsub(".*DR > (\\d+).*", "\\1", validation)) / 100
    FPR_0 <- as.numeric(gsub(".*FPR < (\\d+).*", "\\1", validation)) / 100
    
    # condition 1: DR
    exacttest_1 <- binom.test(x = tp, n = tp + fn, p = DR_0, alternative = "greater")
    suppressWarnings(# no need for chi-squared approximation warnings
      scoretest_1 <-
        prop.test(
          x = tp,
          n = tp + fn,
          p = DR_0,
          alternative = "greater",
          # no continuity correction, fully consistent with how
          # wilson score intervals are computed in diagn_perf()
          correct = FALSE
        ))
    
    # condition 2: FPR
    exacttest_2 <- binom.test(x = fp, n = tn + fp, p = FPR_0, alternative = "less")
    suppressWarnings(# no need for chi-squared approximation warnings
      scoretest_2 <-
        prop.test(
          x = fp,
          n = fp + tn,
          p = FPR_0,
          alternative = "less",
          # no continuity correction, fully consistent with how
          # wilson score intervals are computed in diagn_perf()
          correct = FALSE
        ))
    
    pvals <- tibble(
      exacttest_1 = exacttest_1$p.value,
      scoretest_1 = scoretest_1$p.value,
      exacttest_2 = exacttest_2$p.value,
      scoretest_2 = scoretest_2$p.value
    )  %>% 
      mutate(success = (exacttest_1 < 0.025) & (exacttest_2 < 0.025))
  }
    
  # TODO: other possible conditions
  # } else if(...) {
  # 
  # }

  cat(format(paste0("computed exact p-value(s): ", 
                    format(round(pvals$exacttest_1, 4), nsmall = 4, width = 7),
                    "; ",
                    format(round(pvals$exacttest_2, 4), nsmall = 4, width = 7)
                    ), 
             
             width = 40),
      "( validation altern. hypothesis/-es:", validation, ")\n")
  return(pvals)
}


# run validation hypothesis tests
append_test_results <- function(data, pval_prior_vs_adj){
  
  # find analysis that require prior risks
  requires_prior <- which(data$validation == "DR a.risk > DR prior")
  
  # compute tests
  testresults <- map_dfr(1:nrow(data), function(i){
    # select current row for which to run analysis
    datarow <- data[i,]
    # run analyses that do not need prior risks
    if(!i %in% requires_prior){
      run_validation_tests(data = datarow)
      # run analyses that need prior risks
    } else if(i %in% requires_prior){
      
      analysis_id_prior <- data$Analysis_ID[data$Agorithm_ID == data$Agorithm_ID[i] & # same algorithm ...
                                              data$Cohort_ID == data$Cohort_ID[i] & # ... and same pregnancies ...
                                              endsWith(data$Analysis_ID, "_MFs")] # ... but maternal factors only
      analysis_id_prior <- unique(analysis_id_prior)
      stopifnot(length(analysis_id_prior) == 1L)

      datarow_prior <- data[data$Analysis_ID == analysis_id_prior & data$condition == data$condition[i],]
      stopifnot(nrow(datarow_prior) == 1L)
      
      run_validation_tests(data = datarow, data_prior = datarow_prior, pval_prior_vs_adj = pval_prior_vs_adj)
      
    } 
  })
  
  cat(ncol(testresults), "columns added to the data table\n")
  
  bind_cols(
    df,
    testresults
  )
  
}
