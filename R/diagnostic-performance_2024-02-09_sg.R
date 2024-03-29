# eidted by stefan gehrig, 2024-02-16
# - taken out calculations not needed for FSP validation project
# - renaming of some columns
# - allow to specify a prevalence for NPV / PPV calculations and their intervals
# - added calculations for OAPR
# - allow for absence of positive cases, ensuring constant dimension of contingency table
# - special delta method confidence intervals for prevalence adjusted metrics

################################################################################
## Function diagn_perf                                                        ##
##                                                                            ##
## Computation of diagnostic performance measures with Wilson confidence      ##
## intervals: Sensitivity, Specificity, FPR, PPV, NPV                         ##
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
##                     if NA, prevalence-dependent metrics are skipped        ##
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
                        conf_level=0.95, conf_type="Wilson", n_dig_prop=2, n_dig_lr=2, n_dig_odds=0, ...){
  
  # check if positive class and negative class (entry not positive and not missing) are both found in the respective data columns
  
  
  if( sum( d[,ref_outcome] ==ref_pos,  na.rm=T )==0 &  sum( d[,test_outcome]!=test_pos, na.rm=T )==0 ) { stop("function diagn_perf: neither positive outcome, nor prediction found") }
  if( sum( d[,ref_outcome] ==ref_pos,  na.rm=T )==0 ){ no_cases <- TRUE } else { no_cases <- FALSE }
  if( sum( d[,ref_outcome] !=ref_pos,  na.rm=T )==0 ){ stop("function diagn_perf: negative class not found in reference outcome") }
  if( sum( d[,test_outcome]==test_pos, na.rm=T )==0 ){ no_predicted_cases <- TRUE } else { no_predicted_cases <- FALSE }
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
  
  # ensure constant dimension of contingency table, even in case of missing positive reference class
  d$ref_outcome_bin = factor(d$ref_outcome_bin, levels = c("negative", "positive"))
  d$test_outcome_bin = factor(d$test_outcome_bin, levels = c("negative", "positive"))
  
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
  
  # ad-hoc method for inference on prevalence-corrected contingeny tables: [abandoned in favor of a delta method approach below]
  # create a MODIFIED contintency table, which only changes the prevalence to accord to pre-specified one
  # (the entries are allowed to be non-integer counts; all upcoming statistical functions can deal with this)
  # tn_modified <- (tp+fp+fn+tn) * (1-prevalence) * (tn / (tn+fp))
  # fp_modified <- (tp+fp+fn+tn) * (1-prevalence) * (fp / (tn+fp))
  # fn_modified <- (tp+fp+fn+tn) * (prevalence)   * (fn / (fn+tp))
  # tp_modified <- (tp+fp+fn+tn) * (prevalence)   * (tp / (fn+tp))
  # note: this method should only work well in terms of correct coverage if prevalence in data and 
  # desired prevalence are roughly of comparable magnitude. (prevalence-corrected point estimates
  # for NPV, PPV and OAPR are always correct and are equivalent with the formula from the validation plan.)
  # the "modified sample size" of the proportion estimation for NPV and PPV is not controlled in 
  # this approach (only the full sample size in the contingency table, i.e., sum of margins), which can lead to 
  # over- and underestimation of uncertainty. ideally, one would derive a test or interval
  # from first principles or introduce further constraints on the "modified sample size".

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
  
  
  # False Positive Rate (FPR)
  FPR = binconf(x=fp, n=tn+fp, alpha=1-conf_level, method=CI_method)
  res$FPR[1]        = FPR[1,"PointEst"]
  res$FPR_lwr[1]    = FPR[1,"Lower"]
  res$FPR_upr[1]    = FPR[1,"Upper"]
  res$FPR_str[1] = paste( sprintf( 100*res$FPR[1], fmt=fmt ), 
                          "% (", sprintf( 100*res$FPR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$FPR_upr[1], fmt=fmt ), ")", sep="")
  
  # Screen positive rate (SPR)
  SPR = binconf(x=fp+tp, n=tn+fn+fp+tp, alpha=1-conf_level, method=CI_method)
  res$SPR[1]        = SPR[1,"PointEst"]
  res$SPR_lwr[1]    = SPR[1,"Lower"]
  res$SPR_upr[1]    = SPR[1,"Upper"]
  res$SPR_str[1] = paste( sprintf( 100*res$SPR[1], fmt=fmt ), 
                          "% (", sprintf( 100*res$SPR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$SPR_upr[1], fmt=fmt ), ")", sep="")
  
  
  # Specificity
  spec = binconf(x=tn, n=tn+fp, alpha=1-conf_level, method=CI_method)
  res$Specificity[1] <- sp <- spec[1,"PointEst"]
  res$Specificity_lwr[1]    = spec[1,"Lower"]
  res$Specificity_upr[1]    = spec[1,"Upper"]
  res$Specificity_str[1] = paste( sprintf( 100*res$Specificity[1], fmt=fmt ), 
                                  "% (", sprintf( 100*res$Specificity_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$Specificity_upr[1], fmt=fmt ), ")", sep="")
  
  if(!no_cases){
   
    # DR
    sens = binconf(x=tp, n=tp+fn, alpha=1-conf_level, method=CI_method)
    res$DR[1] <- se <- sens[1,"PointEst"]
    res$DR_lwr[1]    = sens[1,"Lower"]
    res$DR_upr[1]    = sens[1,"Upper"]
    res$DR_str[1] = paste( sprintf( 100*res$DR[1], fmt=fmt ), 
                           "% (", sprintf( 100*res$DR_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$DR_upr[1], fmt=fmt ), ")", sep="")
    
    if(!is.na(prevalence)){
      # NPV at a specified prevalence
      NPV_at_prevalence <- delta_log(tp, fp, fn, tn, prevalence, "npv", conf_level)
      
      res$NPV_at_prevalence[1]     = sp * (1 - prevalence) / (sp * (1 - prevalence) + (1 - se) * prevalence)
      res$NPV_at_prevalence_lwr[1] = NPV_at_prevalence$lwr
      res$NPV_at_prevalence_upr[1] = NPV_at_prevalence$upr
      res$NPV_at_prevalence_str[1] = paste( sprintf( 100*res$NPV_at_prevalence[1], fmt=fmt ),
                                            "% (", sprintf( 100*res$NPV_at_prevalence_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$NPV_at_prevalence_upr[1], fmt=fmt ), ")", sep="")
      }

    
    if(!no_predicted_cases){
      
      if(!is.na(prevalence)){
        # PPV at a specified prevalence
        PPV_at_prevalence <- delta_log(tp, fp, fn, tn, prevalence, "ppv", conf_level)
        
        res$PPV_at_prevalence[1]     = se * prevalence / (se * prevalence + (1 - sp) * (1-prevalence))
        res$PPV_at_prevalence_lwr[1] = PPV_at_prevalence$lwr
        res$PPV_at_prevalence_upr[1] = PPV_at_prevalence$upr
        res$PPV_at_prevalence_str[1] = paste( sprintf( 100*res$PPV_at_prevalence[1], fmt=fmt ),
                                              "% (", sprintf( 100*res$PPV_at_prevalence_lwr[1], fmt=fmt ), ", ", sprintf( 100*res$PPV_at_prevalence_upr[1], fmt=fmt ), ")", sep="")
        
        # OAPR
        res$OAPR_at_prevalence[1] <- res$PPV_at_prevalence[1] / (1-res$PPV_at_prevalence[1])
        
        if(!res$PPV_at_prevalence[1] %in% c(0,1)){
          
          OAPR_at_prevalence <- delta_log(tp, fp, fn, tn, prevalence, "oapr", conf_level)
          stopifnot(round(res$OAPR_at_prevalence[1], 8) == round(OAPR_at_prevalence$est, 8))
          
          res$OAPR_at_prevalence_lwr[1] = OAPR_at_prevalence$lwr
          res$OAPR_at_prevalence_upr[1] = OAPR_at_prevalence$upr
          res$OAPR_at_prevalence_str[1] = paste0("1:", sprintf( janitor::round_half_up(1/res$OAPR_at_prevalence[1],n_dig_odds), fmt=fmt_odds ))
        }
      }

    }
    
  }
  
  
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
