## FSP Validation analyses (Feb 2024)

**Goal:** Conduct FastScreen PLUS validation analysis according to validation plan.

**Note:** The folder "FSP_Exchange" is a copy of the shared folder on Share Point. It should by synchronized to contain the most recent data and analysis sheets.

**Usage:** 

The overarching script, which runs a complete analysis, is `run_master.R`. It allows to specify which analysis IDs schould be run, and whether $p$-values for the comparison of prior and posterior detection rates are of interest. The configuration of the validation analyses (which endpoints to analyze, which data sets to use, which tests to run, ...) needs to be specified in `FSP_Exchange/data/Overview_Stat_Analyses.xlsx`. All data files that are mentioned in the configuration need to be placed in the respective directories in `FSP_Exchange/data/PE` (for pre-eclampsyia endpoints) or `FSP_Exchange/data/Trisomy` (for trisomy endpoints), such that they can be automatically imported for analysis.

When successfully running `run_master.R`, three output files are exported to `/results`, with current date and time pasted into the file name:

- a HTML log file of the R session console output
- an Excel file, which appends all analysis results to the information stored in the configuration table `FSP_Exchange/data/Overview_Stat_Analyses.xlsx` (the results table is in the "long" format compared to the configuration table, where different endpoints of the same analysis are stored in a "wide" format).
- An `.rds` file, which contains the same information as the Excel file, but also the cleaned, transformed and merged data, on which each analysis was run (in a list column). This file provides the easiest way to quickly reproduce or investigate the results.

The `run_master.R` script under the hood sources the following scripts:

- `R/master.R`: the analyasis pipeline
- `R/functions.R`: analysis functions called in the pipeline
- `R/diagnostic-performance_2024-02-09_sg.R`: function `diagn_perf()`, an edited version of the original function provided by BRAHMS at the start of the project. Major edits are:

    - inclusion of OAPR as performance metric
    - allow for a prevalence-adjusted NPV / PPV / OAPR
    - a bit more liberal with respect to requirements (e.g., allow for absence of positive cases)
    - confidence intervals for OAPR and prevalence adjusted NPV / PPV via delta method on the log scale (function `delta_log()` in `R/functions.R`)
    
- `R/run_master_log.Rmd`: the R markdown template for the log file

**Output columns:**

The results table contains all performance metric results as additional columns, as well as some further added columns:

- `validation`: the validation requirement(s) which is/are tested in the analysis (if any)
- `type`: outcome type under analysis (PE or trisomy)
- `cutoff_numeric`: the cutoff used for performance calculation, as a real number (depending on the analysis configuration, it is either pre-specified or computed according to certain requirements, e.g., a specific FPR which must not be exceeded)
- `prevalence_numeric`: the prevalence, as a real number, at which prevalence-dependent performance metrics (PPV, NPV, OAPR) are estimated (is missing in case no prevalence has been pre-specified; in that case, prevalence-dependent performance metrics are missing as well)
- `exacttest_1`: $p$-value of exact binomial test for first/only alternative hypothesis (stored in column `validation`)
- `exacttest_2`: $p$-value of exact binomial test for second (if there is a second) alternative hypothesis (stored in column `validation`)
- `scoretest_1`: $p$-value of score test for first/only alternative hypothesis (stored in column `validation`).  The (non-continuity corrected) score test $p$-values are mainly supplied because they are consistent with the Wilson score confidence intervals provided for some performance metrics, like detection rate. They are less conservative than exact binomial tests.
- `scoretest_2`: $p$-value of score test for second (if there is a second) alternative hypothesis (stored in column `validation`)
- `success`: indicates whether validation test(s) (stored in column `validation`) is/are successful. For null hypothesis tests, they are based on the $p$-values of exact binomial tests and $\alpha = 0.025$.

**Some useful details**:

- If two twins that can have different outcomes are supplied in the same analysis and its data sheets, only the first one is analyzed
- If multiple cutoffs lead to the same desired FPR, the one which maximizes detection rate (i.e., the smallest cutoff among them) is chosen
- If more than two endpoints occur in the same data sets (e.g., "Con", "PE<32 WE", "PE<34 WE", and "PE<37 WE"), those non-conventional observations which do not correspond to the endpoint of interest in the analysis are removed. That is, we always compare the endpoint of interest only with "Con" outcomes.
