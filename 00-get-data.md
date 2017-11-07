Download raw data
================
Tristan Mahr
2017-11-07

-   [Simple tables](#simple-tables)
-   [Prepare eyetracking database tables](#prepare-eyetracking-database-tables)
-   [Download eyetracking queries](#download-eyetracking-queries)

This script downloads the raw data from our lab's private database. It cannot be reproduced by people outside our lab. Therefore, I try to keep the script brief and have it save the needed data into the folder `data-raw`.

Simple tables
-------------

Create or locate necessary infrastructure.

``` r
library(dplyr, warn.conflicts = FALSE)
library(L2TDatabase)

# Create a directory to store the data
create_directory <- function(path) {
  if (!dir.exists(path)) {
    message("Creating directory: ", path)
    dir.create(path, showWarnings = FALSE)
  }
  invisible(path)
}

# Try to find the file with L2T database information in user's home folder or in
# this repository
find_database_config <- function() {
  home_config <- path.expand("~/l2t_db.cnf")
  repo_config <- rprojroot::find_rstudio_root_file("l2t_db.cnf")

  if (file.exists(home_config)) {
    home_config
  } else if (file.exists(repo_config)) {
    repo_config
  } else {
    stop("Cannot find `l2t_db.cnf` file")
  }
}

create_directory("./data-raw")
create_directory("./data")
```

Get test scores.

``` r
# Connect to database
l2t_main <- l2t_connect(find_database_config(), "l2t")

# Download matches and scores from various tests
df_matches <- tbl(l2t_main, "CIMatching") %>%
  left_join(tbl(l2t_main, "EVT")) %>%
  left_join(tbl(l2t_main, "PPVT")) %>%
  left_join(tbl(l2t_main, "GFTA")) %>%
  select(Group = Matching_Group, Matching_PairNumber, ChildStudyID,
         Study, ResearchID, ChildID,
         Female, AAE, LateTalker, CImplant,
         Maternal_Education, Maternal_Education_Level,
         EVT_Age, EVT_Raw, EVT_GSV, EVT_Standard,
         PPVT_Age, PPVT_Raw, PPVT_GSV, PPVT_Standard,
         GFTA_Age:GFTA_Standard) %>%
  collect()
readr::write_csv(df_matches, file.path("data-raw", "test-scores.csv"))
```

Prepare eyetracking database tables
-----------------------------------

This is a tedious step. We need to download the eyetracking data. First, let's connect to the database and prepare some queries.

``` r
l2t_eyetracking <- l2t_connect(find_database_config(), "eyetracking")

# We could do this with the prepared queries (q_BlocksByStudy, etc.), but the
# query q_LooksByStudy takes forever to run. So we will select identify the
# blocks we want and get the trials and looks for just those blocks. That should
# be faster than start with all the data and narrowing down to the subset we
# want.

df_child_info <- df_matches %>%
  select(Group:ResearchID)

# Find the numbers of the blocks
tbl_blocks <- tbl(l2t_eyetracking, "Blocks") %>%
  filter(ChildStudyID %in% df_matches$ChildStudyID,
         Block_Task == "MP") %>%
  select(BlockID, ChildStudyID, Block_Basename,
         Block_DateTime, Block_Version, Block_Age)

# Get the attributes for these blocks
tbl_blocks_attrs <- tbl(l2t_eyetracking, "BlockAttributes") %>%
  inner_join(tbl_blocks) %>%
  # Exclude unimportant attributes
  filter(!(BlockAttribute_Name %in% c("RandomSeed", "FrameRate",
                                      "ScreenHeight", "ScreenWidth"))) %>%
  select(ChildStudyID, BlockID, BlockAttribute_Name, BlockAttribute_Value)

# Get trial id numbers for these blocks
tbl_trials <- tbl(l2t_eyetracking, "Trials") %>%
  inner_join(tbl_blocks) %>%
  select(ChildStudyID, BlockID, TrialID, Trial_TrialNo)

# Get attributes of the trials
tbl_trials_attrs <- tbl(l2t_eyetracking, "TrialAttributes") %>%
  inner_join(tbl_trials) %>%
  select(ChildStudyID, BlockID, TrialID,
         TrialAttribute_Name, TrialAttribute_Value)

# The big one. Get the looking data for these trials.
tbl_looks <- tbl(l2t_eyetracking, "Looks") %>%
  inner_join(tbl_trials) %>%
  select(ChildStudyID, BlockID, TrialID, Time, GazeByImageAOI, GazeByAOI)
```

Download eyetracking queries
----------------------------

``` r
df_blocks <- collect(tbl_blocks) %>%
  left_join(df_child_info) %>%
  group_by(ChildStudyID) %>%
  # We want one age per child, so use earliest.
  mutate(Block_Age = min(Block_Age)) %>%
  ungroup()

# Get the dialect and stimulus version
df_blocks_attrs <- collect(tbl_blocks_attrs) %>%
  # Pivot from long to wide so have the attributes we want
  tidyr::spread(BlockAttribute_Name, BlockAttribute_Value) %>%
  select(ChildStudyID, BlockID,
         Block_Dialect = Dialect, StimulusSet)

# Add dialect to block info
df_blocks <- df_blocks %>%
  # My convention is to put the database ids first, with the slowest varying
  # ones first.
  select(ChildStudyID, BlockID, Study, ResearchID, Block_Age,
         Block_Basename, Block_Version) %>%
  left_join(df_blocks_attrs)


df_trials <- collect(tbl_trials)
df_trials_attrs <- collect(tbl_trials_attrs, n = Inf)

df_trial_info <- df_trials %>%
  left_join(df_blocks) %>%
  left_join(df_trials_attrs) %>%
  tidyr::spread(TrialAttribute_Name, TrialAttribute_Value) %>%
  select(BlockID, TrialID, Study, ResearchID, Block_Basename,
         TrialNo = Trial_TrialNo,
         Condition = StimType, WordGroup, TargetWord, Bias_ImageAOI,
         DistractorImage, FamiliarImage, UnfamiliarImage,
         ImageL, ImageR, TargetImage)

df_looks <- collect(tbl_looks, n = Inf)

df_looks <- df_blocks %>%
  left_join(df_trials) %>%
  left_join(df_looks) %>%
  select(-Block_Age, -Block_Basename, -Block_Version,
         -Block_Dialect, -StimulusSet, -ChildStudyID) %>%
  select(BlockID, TrialID, Study, ResearchID, TrialNo = Trial_TrialNo,
         Time, GazeByImageAOI, GazeByAOI)

readr::write_csv(df_looks, file.path("data-raw", "looks.csv.gz"))
readr::write_csv(df_trial_info, file.path("data-raw", "trials.csv"))
readr::write_csv(df_blocks, file.path("data-raw", "blocks.csv"))
```

------------------------------------------------------------------------

``` r
sessioninfo::session_info()
#> - Session info -----------------------------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.4.1 (2017-06-30)
#>  os       Windows 7 x64 SP 1          
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United States.1252  
#>  tz       America/Chicago             
#>  date     2017-11-07                  
#> 
#> - Packages ---------------------------------------------------------------------------------------
#>  package     * version    date       source                                     
#>  assertthat    0.2.0      2017-04-11 CRAN (R 3.3.2)                             
#>  backports     1.1.1      2017-09-25 CRAN (R 3.4.1)                             
#>  bindr         0.1        2016-11-13 CRAN (R 3.4.0)                             
#>  bindrcpp      0.2        2017-06-17 CRAN (R 3.4.0)                             
#>  clisymbols    1.2.0      2017-08-04 Github (gaborcsardi/clisymbols@e49b4f5)    
#>  DBI           0.7        2017-06-18 CRAN (R 3.4.0)                             
#>  dbplyr        1.1.0      2017-06-27 CRAN (R 3.4.1)                             
#>  digest        0.6.12     2017-01-27 CRAN (R 3.3.2)                             
#>  dplyr       * 0.7.4      2017-09-28 CRAN (R 3.4.2)                             
#>  evaluate      0.10.1     2017-06-24 CRAN (R 3.4.1)                             
#>  glue          1.2.0      2017-10-29 CRAN (R 3.4.2)                             
#>  hms           0.3        2016-11-22 CRAN (R 3.3.2)                             
#>  htmltools     0.3.6      2017-04-28 CRAN (R 3.4.0)                             
#>  knitr       * 1.17       2017-08-10 CRAN (R 3.4.2)                             
#>  L2TDatabase * 0.1        2017-08-25 Github (LearningToTalk/L2TDatabase@18c957b)
#>  lubridate     1.7.1      2017-11-03 CRAN (R 3.4.2)                             
#>  magrittr      1.5        2014-11-22 CRAN (R 3.1.2)                             
#>  pkgconfig     2.0.1      2017-03-21 CRAN (R 3.3.3)                             
#>  purrr         0.2.4      2017-10-18 CRAN (R 3.4.2)                             
#>  R6            2.2.2      2017-06-17 CRAN (R 3.4.0)                             
#>  Rcpp          0.12.13    2017-09-28 CRAN (R 3.4.2)                             
#>  readr         1.1.1      2017-05-16 CRAN (R 3.4.0)                             
#>  rlang         0.1.4      2017-11-05 CRAN (R 3.4.2)                             
#>  rmarkdown     1.6        2017-06-15 CRAN (R 3.4.2)                             
#>  RMySQL        0.10.13    2017-08-14 CRAN (R 3.4.1)                             
#>  rprojroot     1.2        2017-01-16 CRAN (R 3.3.2)                             
#>  sessioninfo   1.0.1      2017-09-13 Github (r-lib/sessioninfo@e813de4)         
#>  stringi       1.1.5      2017-04-07 CRAN (R 3.3.3)                             
#>  stringr       1.2.0      2017-02-18 CRAN (R 3.3.2)                             
#>  tibble        1.3.4      2017-08-22 CRAN (R 3.4.1)                             
#>  tidyr         0.7.2      2017-10-16 CRAN (R 3.4.2)                             
#>  tidyselect    0.2.3      2017-11-06 CRAN (R 3.4.2)                             
#>  withr         2.1.0.9000 2017-11-02 Github (jimhester/withr@8ba5e46)           
#>  yaml          2.1.14     2016-11-12 CRAN (R 3.4.2)
```
