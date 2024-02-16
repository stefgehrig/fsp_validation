# eidted by stefan gehrig, 2024-02-16
# - taken out calculations not needed for FSP validation project
# - renaming of some columns
# - allow to specify a prevalence for NPV / PPV calculations and their intervals
# - added calculations for OAPR

################################################################################
## Function diagn_perf                                                        ##
##                                                                            ##
## Computation of diagnostic performance measures with Wilson confidence      ##
## intervals: Sensitivity, Specificity, FPR, PPV, NPV, LR+, LR-               ##
##                                                                            ##
## Input:                                                                     ##
##    +  d:            dataframe from which to extract the data               ##
##    +  ref_outcome:  name of binary column with true outcome classes        ##
##    +  ref_pos:      value of true_outcome to be considered positive -      ##
##                     all other values are considered negative)              ##
##    +  test_outcome: name of binary column with predicted outcome classes   ##
##    +  test_pos:     value of pred_outcome to be considered positive -      ##
##                     all other values are considered negative)              ##
##    +  prevalence:   prevalence at which NPV and PPV are compuated;         ##
##                     cannot be left empty.                                  ##
##    +  conf_level:   level for 2-sided confidence intervals                 ##
##    +  conf_type:    CIs of Sensitivity, Specificity, FPR, PPV, NPV         ##
##                     according to "Wilson" or acc. to "Clopper-Pearson"     ##
##                     (LR+ & LR- by normal approximation on log-transformed  ##
##                      scale)                                                ##
##    +  n_dig_prop:   number of digits for pretty print output for           ##
##                     Sensitivity, Specificity, FPR, PPV, NPV                ##
##    +  n_dig_lr:     number of digits for pretty print output for           ##
##                     LR+, LR-                                               ##
##    +  n_dig_odds:   number of digits for odds                              ##
##                                                                            ##
## Output:                                                                    ##
##    +  dataframe with one row                                               ##
##       (1) input specifications, (2) performance measures,                  ##
##       (3) counts of contingency table                                      ##
##                                                                            ##
## Needed R-packages:                                                         ##
##    +  Hmisc                                                                ##
##                                                                            ##
## Future development plans:                                                  ##
##    + chi-square test & Fisher exact test                                   ##
##                                                                            ##
## Jan Wiemer, Thermo Fisher Scientific, 2022-01-28                           ##
##                                                                            ##
##                                                                            ##
################################################################################
diagn_perf <- function( d, ref_outcome, ref_pos, test_outcome, test_pos, 
                        prevalence,
                        conf_level=0.95, conf_type="Wilson", n_dig_prop=2, n_dig_lr=2, n_dig_odds=3, ...){
  
  # check if positive class and negative class (entry not positive and not missing) are both found in the respective data columns
  if( sum( d[,ref_outcome] ==ref_pos,  na.rm=T )==0 ){ stop("function diagn_perf: positive class not found in reference outcome") }
  if( sum( d[,ref_outcome] !=ref_pos,  na.rm=T )==0 ){ stop("function diagn_perf: negative class not found in reference outcome") }
  if( sum( d[,test_outcome]==test_pos, na.rm=T )==0 ){ stop("function diagn_perf: positive class not found in test outcome") }
  if( sum( d[,test_outcome]!=test_pos, na.rm=T )==0 ){ stop("function diagn_perf: negative class not found in test outcome") }
  # for testing stopping conditions:
  # ref_pos="Yes"
  # ref_outcome <- "outspe2"
  # d$outspe2 = ifelse( d$outspe=="Yes", as.character(d$outspe), NA) # no entry for negative class (only "Yes" or "NA")
  # d$outspe2 = ifelse( d$outspe=="Yes", NA, as.character(d$outspe)) # no entry "Yes" for positive class
  # d$outspe2[sample(x=nrow(d),size=30)] <- NA
  # table(new=d$outspe2, old=d$outspe, useNA="a")
    
  #---------------------------------------------------------------------------------------------.
  #### data processing ####
  
  # fixed reference variable for contingency table
  d$ref_outcome_bin <- ""
  d$ref_outcome_bin[ d[,ref_outcome]==ref_pos ] <- "positive"
  d$ref_outcome_bin[ d[,ref_outcome]!=ref_pos ] <- "negative"
  d$ref_outcome_bin[ is.na(d[,ref_outcome]) ]   <- NA
  #addmargins( table( new=d$ref_outcome_bin, old=t(d[,ref_outcome]), useNA="a"))

  # fixed test variable for contingency table
  d$test_outcome_bin <- ""
  d$test_outcome_bin[ d[,test_outcome]==test_pos ] <- "positive"
  d$test_outcome_bin[ d[,test_outcome]!=test_pos ] <- "negative"
  d$test_outcome_bin[ is.na(d[,test_outcome]) ]   <- NA
  #addmargins( table( new=d$test_outcome_bin, old=t(d[,test_outcome]), useNA="a"))
  
  #---------------------------------------------------------------------------------------------.
  # results table format ####
  na_num = NA*0
  res <- tibble_row(
    # ref_outcome = ref_outcome,
    # ref_pos = ref_pos,
    # test_outcome = test_outcome,
    # test_pos = test_pos,
    # conf_level = conf_level,
    # conf_type = conf_type,
    DR = na_num,
    DR_lwr = na_num,
    DR_upr = na_num,
    DR_str = "",
    FPR = na_num,
    FPR_lwr = na_num,
    FPR_upr = na_num,
    FPR_str = "",
    SPR = na_num,
    SPR_lwr = na_num,
    SPR_upr = na_num,
    SPR_str = "",
    Specificity = na_num,
    Specificity_lwr = na_num,
    Specificity_upr = na_num,
    Specificity_str = "",
    PPV_at_prevalence = na_num,
    PPV_at_prevalence_lwr = na_num,
    PPV_at_prevalence_upr = na_num,
    PPV_at_prevalence_str = "",
    NPV_at_prevalence = na_num,
    NPV_at_prevalence_lwr = na_num,
    NPV_at_prevalence_upr = na_num,
    NPV_at_prevalence_str = "",
    OAPR_at_prevalence = na_num,
    OAPR_at_prevalence_lwr = na_num,
    OAPR_at_prevalence_upr = na_num,
    OAPR_at_prevalence_str = "",
    # PLR = na_num,
    # LCI_PLR = na_num,
    # UCI_PLR = na_num,
    # PLR_str = "",
    # NLR = na_num,
    # LCI_NLR = na_num,
    # UCI_NLR = na_num,
    # NLR_str = "",
    # p_val_chisq = na_num,
    # p_val_fisher = na_num,
    tp = na_num,
    fp = na_num,
    fn = na_num,
    tn = na_num,
    n_ref_pos = na_num,
    n_ref_neg = na_num,
    n_test_pos = na_num,
    n_test_neg = na_num,
    n_ref = na_num,
    mis_ref = na_num,
    n_test = na_num,
    mis_test = na_num,
    n_ref_and_test = na_num,
    n_total = na_num,
    mis_ref_or_test = na_num
  )
  
  #---------------------------------------------------------------------------------------------.
  # contingency table and read-out numbers ####
  ct = addmargins( table( test=d$test_outcome_bin, reference=d$ref_outcome_bin, useNA="a" ) )
  # colnames(ct)
  # rownames(ct)
  
  res$tp[1] <- tp <- ct["positive","positive"]
  res$fp[1] <- fp <- ct["positive","negative"]
  res$fn[1] <- fn <- ct["negative","positive"]
  res$tn[1] <- tn <- ct["negative","negative"]

  res$n_ref_pos[1] <- n_ref_pos <- ct["Sum","positive"]
  res$n_ref_neg[1] <- n_ref_neg <- ct["Sum","negative"]
  res$n_ref[1]     = n_ref_pos + n_ref_neg
  res$mis_ref[1]   = ct["Sum", 3]          # column 3 for NA's
  
  res$n_test_pos[1] <- n_test_pos <- ct["positive","Sum"]
  res$n_test_neg[1] <- n_test_neg <- ct["negative","Sum"]
  res$n_test[1]     = n_test_pos + n_test_neg
  res$mis_test[1]   = ct[3,         "Sum"] # row 3 for NA's
  
  res$n_ref_and_test[1]  <- n_ref_and_test <- tp + fp + fn + tn
  res$n_total[1] <- n_total <- ct["Sum","Sum"]
  res$mis_ref_or_test[1] = n_total - n_ref_and_test
  
  # create a MODIFIED contintency table, which only changes the prevalence to accord to pre-specified one
  # (the table is allowed to have non-integer counts; all upcoming statistical functions can deal with this)
  ct_modified <- ct
  ct_modified["Sum", "negative"]     <- ct["Sum","Sum"] * (1-prevalence)
  ct_modified["Sum", "positive"]     <- ct["Sum","Sum"] * (prevalence)
  ct_modified["negative","negative"] <- ct["Sum","Sum"] * (1-prevalence) * (ct["negative", "negative"] / ct["Sum", "negative"])
  ct_modified["positive","negative"] <- ct["Sum","Sum"] * (1-prevalence) * (ct["positive", "negative"] / ct["Sum", "negative"])
  ct_modified["negative","positive"] <- ct["Sum","Sum"] * (prevalence) * (ct["negative", "positive"] / ct["Sum", "positive"])
  ct_modified["positive","positive"] <- ct["Sum","Sum"] * (prevalence) * (ct["positive", "positive"] / ct["Sum", "positive"])
  tp_modified <- ct_modified["positive","positive"]
  fp_modified <- ct_modified["positive","negative"]
  fn_modified <- ct_modified["negative","positive"]
  tn_modified <- ct_modified["negative","negative"]
  

  #---------------------------------------------------------------------------------------------.
  # performance measures: DR, Specificity, FPR, PPV, NPV ####
  
  if(conf_type %in% c("Wilson","wilson","W","w")){ 
    CI_method = "wilson" 
    } else if(conf_type %in% c("Clopper-Pearson","clopper-pearson","CP","cp")){
    CI_method = "exact"
  }
    
  # formatting
  fmt = paste("%.",n_dig_prop,"f", sep="")
  fmt_odds = paste("%.",n_dig_odds,"f", sep="")
  
  # DR
  sens = binconf(x=tp, n=tp+fn, alpha=1-conf_level, method=CI_method)
  res$DR[1] <- se <- sens[1,"PointEst"]
  res$DR_lwr[1]    = sens[1,"Lower"]
  res$DR_upr[1]    = sens[1,"Upper"]
  res$DR_str[1] = paste( sprintf( 100*res$DR[1], fmt=fmt ), 
                               "% (", sprintf( 100*res$DR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$DR_upr[1], fmt=fmt ), ")", sep="")
    
  # Specificity
  spec = binconf(x=tn, n=tn+fp, alpha=1-conf_level, method=CI_method)
  res$Specificity[1] <- sp <- spec[1,"PointEst"]
  res$Specificity_lwr[1]    = spec[1,"Lower"]
  res$Specificity_upr[1]    = spec[1,"Upper"]
  res$Specificity_str[1] = paste( sprintf( 100*res$Specificity[1], fmt=fmt ), 
                               "% (", sprintf( 100*res$Specificity_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$Specificity_upr[1], fmt=fmt ), ")", sep="")

  # Screen positive rate (SPR)
  SPR = binconf(x=fp+tp, n=tn+fn+fp+tp, alpha=1-conf_level, method=CI_method)
  res$SPR[1]        = SPR[1,"PointEst"]
  res$SPR_lwr[1]    = SPR[1,"Lower"]
  res$SPR_upr[1]    = SPR[1,"Upper"]
  res$SPR_str[1] = paste( sprintf( 100*res$SPR[1], fmt=fmt ), 
                          "% (", sprintf( 100*res$SPR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$SPR_upr[1], fmt=fmt ), ")", sep="")
  
  # False Positive Rate (FPR)
  FPR = binconf(x=fp, n=tn+fp, alpha=1-conf_level, method=CI_method)
  res$FPR[1]        = FPR[1,"PointEst"]
  res$FPR_lwr[1]    = FPR[1,"Lower"]
  res$FPR_upr[1]    = FPR[1,"Upper"]
  res$FPR_str[1] = paste( sprintf( 100*res$FPR[1], fmt=fmt ), 
                                  "% (", sprintf( 100*res$FPR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$FPR_upr[1], fmt=fmt ), ")", sep="")
  
  # PPV at a specified prevalence
  PPV_at_prevalence <- binconf(x=tp_modified, n=tp_modified+fp_modified, alpha=1-conf_level, method=CI_method)

  stopifnot( # check the point estimation approach implemented here against formula in validation plan. they should be equivalent
    round(PPV_at_prevalence[1,"PointEst"], 12) ==  round(se * prevalence / (se * prevalence + (1 - sp) * (1-prevalence)), 12)
  )
  
  res$PPV_at_prevalence[1]     = PPV_at_prevalence[1,"PointEst"]
  res$PPV_at_prevalence_lwr[1] = PPV_at_prevalence[1,"Lower"]
  res$PPV_at_prevalence_upr[1] = PPV_at_prevalence[1,"Upper"]
  res$PPV_at_prevalence_str[1] = paste( sprintf( 100*res$PPV_at_prevalence[1], fmt=fmt ),
                               "% (", sprintf( 100*res$PPV_at_prevalence_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$PPV_at_prevalence_upr[1], fmt=fmt ), ")", sep="")
  
  # NPV at a specified prevalence
  NPV_at_prevalence <- binconf(x=tn_modified, n=tn_modified+fn_modified, alpha=1-conf_level, method=CI_method)
  
  stopifnot( # check the point estimation approach implemented here against formula in validation plan. they should be equivalent
    round(NPV_at_prevalence[1,"PointEst"], 12) == round(sp * (1 - prevalence) / (sp * (1 - prevalence) + (1 - se) * prevalence), 12)
  )
  
  res$NPV_at_prevalence[1]     = NPV_at_prevalence[1,"PointEst"]
  res$NPV_at_prevalence_lwr[1] = NPV_at_prevalence[1,"Lower"]
  res$NPV_at_prevalence_upr[1] = NPV_at_prevalence[1,"Upper"]
  res$NPV_at_prevalence_str[1] = paste( sprintf( 100*res$NPV_at_prevalence[1], fmt=fmt ),
                                        "% (", sprintf( 100*res$NPV_at_prevalence_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$NPV_at_prevalence_upr[1], fmt=fmt ), ")", sep="")
  
  # OAPR
  res$OAPR_at_prevalence[1] <- res$PPV_at_prevalence[1] / (1-res$PPV_at_prevalence[1])
    
    stopifnot( # check the point estimation approach implemented here against formula in validation plan. they should be equivalent
      round(res$OAPR_at_prevalence[1], 12) == round( se / ((1-sp) * (1-prevalence)) * prevalence, 12)
    )
  res$OAPR_at_prevalence_lwr[1] = res$PPV_at_prevalence_lwr[1] / (1-res$PPV_at_prevalence_lwr[1])
  res$OAPR_at_prevalence_upr[1] = res$PPV_at_prevalence_upr[1] / (1-res$PPV_at_prevalence_upr[1])
  res$OAPR_at_prevalence_str[1] = paste( sprintf( res$OAPR_at_prevalence[1], fmt=fmt_odds ),
                                        " (", sprintf( res$OAPR_at_prevalence_lwr[1], fmt=fmt_odds ), ", ", sprintf( res$OAPR_at_prevalence_upr[1], fmt=fmt_odds ), ")", sep="")
  
  # #---------------------------------------------------------------------------------------------.
  # # Diagnostic likelihood ratios with normal approximation
  # # formulas for CI-computation taken from R-package reportROC 
  # # e.g. https://github.com/cran/reportROC/blob/master/R/reportROC.R
  # 
  # # PLR: positive likelihood ratio
  # fmt_lr = paste("%.",n_dig_lr,"f", sep="")
  # q = qnorm( p = 1 - (1 - conf_level)/2 )
  # res$PLR[1] <- PLR <- se / (1 - sp)
  # factor_CI = exp(  q * sqrt( (1-se) / tp + sp / fp ) )  # normal approximation of log( PLR )
  # res$LCI_PLR[1] = PLR / factor_CI
  # res$UCI_PLR[1] = PLR * factor_CI
  # res$PLR_str[1] = paste( sprintf( res$PLR[1], fmt=fmt_lr ), 
  #                         " (", sprintf( res$LCI_PLR[1], fmt=fmt_lr ), ", ", sprintf( res$UCI_PLR[1], fmt=fmt_lr ), ")", sep="")
  # 
  # # NLR: positive likelihood ratio
  # res$NLR[1] <- NLR <- ( 1 - se ) / sp
  # factor_CI = exp(  q * sqrt( se / fn + ( 1 - sp ) / tn ) )  # normal approximation of log( NLR )
  # exp( 1.96*sqrt(se/fn+(1-sp)/tn))
  # res$LCI_NLR[1] = NLR / factor_CI
  # res$UCI_NLR[1] = NLR * factor_CI
  # res$NLR_str[1] = paste( sprintf( res$NLR[1], fmt=fmt_lr ), 
  #                         " (", sprintf( res$LCI_NLR[1], fmt=fmt_lr ), ", ", sprintf( res$UCI_NLR[1], fmt=fmt_lr ), ")", sep="")
  # 
  # #---------------------------------------------------------------------------------------------.
  # # statistical tests for contingency table (complete case analysis) ####
  # res$p_val_chisq[1]  = chisq.test(  x=matrix( c(tp,fp,fn,tn), nrow=2, ncol=2)                          )$p.value
  # res$p_val_fisher[1] = fisher.test( x=matrix( c(tp,fp,fn,tn), nrow=2, ncol=2), alternative="two.sided" )$p.value

  ## Return output table
  return(res)
}

# # ---------------------------------------------------------------------------------------------.
# # # Examples for testing ####
# #  function tested on 2022-02-08 by Jan Wiemer:
# #   + function run three times for two data settings: (a) without and (b) with missing values
# #   + results compared with table output printed to console:
# #     review correct counts, compare performance estimates with manual computations
# # 
# library(dplyr)
# 
# # Create dataset
# set.seed(1529)
# cl = c("pos","neg")
# result <- NULL
# for(i in 1:3){
#   reference_results = sample(x=cl, size=100, replace=T)
#   bm_results        = sample(x=cl, size=100, replace=T)
#   d1 <- tibble( reference_results, bm_results )
#   print( addmargins( table( ref=d1$reference_results, bm=d1$bm_results, useNA="a") ) )
#   # debug(diagn_perf)
#   result <- bind_rows( result, diagn_perf( d1, ref_outcome="reference_results", ref_pos="pos", test_outcome="bm_results", test_pos="pos" ) )
#   # undebug(diagn_perf)
# }
# write_csv(result, file="../results/diagnostic-performance_test-run_2022-02-08.csv")
# 
# set.seed(1529)
# cl = c("pos","neg")
# result <- NULL
# for(i in 1:3){
#   reference_results = sample(x=cl, size=150, replace=T)
#   reference_results[sample(150,20)] <- NA
#   bm_results        = sample(x=cl, size=150, replace=T)
#   bm_results[sample(150,50)] <- NA
#   d1 <- tibble( reference_results, bm_results )
#   print( addmargins( table( ref=d1$reference_results, bm=d1$bm_results, useNA="a") ) )
#   # debug(diagn_perf)
#   result <- bind_rows( result, diagn_perf( d1, ref_outcome="reference_results", ref_pos="pos", test_outcome="bm_results", test_pos="pos" ) )
#   # undebug(diagn_perf)
# }
# write_csv(result, file="../results/diagnostic-performance_test-run2_2022-02-08.csv")
# 
# res1 = t(diagn_perf( d1, ref_outcome="reference_results", ref_pos="pos", test_outcome="bm_results", test_pos="pos", conf_level=0.9, n_dig_prop=1, n_dig_lr=5 ))
# res2 = t(diagn_perf( d1, ref_outcome="reference_results", ref_pos="pos", test_outcome="bm_results", test_pos="pos", conf_level=0.9, conf_type="Clopper-Pearson", n_dig_prop=1, n_dig_lr=5 ))

#---------------------------------------------------------------------------------------------.
# examplary specification of variables for PRAECIS data

# d = die
# ref_outcome = "outspe"
# ref_pos = "Yes"
# test_outcome = "ratio_binc"
# test_pos = "positive"
# conf_level=0.95
# conf_type="Wilson"
# n_dig_prop=2

