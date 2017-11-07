Data screening
================
Tristan Mahr
2017-11-07

-   [Set up](#set-up)
-   [Remove blocks from the bad version of the experiment](#remove-blocks-from-the-bad-version-of-the-experiment)
-   [Find unreliable trials](#find-unreliable-trials)
-   [Find unreliable blocks](#find-unreliable-blocks)
-   [Enforce minimum number of trials per condition](#enforce-minimum-number-of-trials-per-condition)
-   [Remove unpaired children](#remove-unpaired-children)
-   [Data screening counts](#data-screening-counts)
-   [Clean up](#clean-up)

This script screens the eyetracking data by removing unreliable trials and unreliable blocks of trials. It preserves the pairs of matched children. (If a child is removed for have no reliable eyetracking data, their match is also removed.) Counts of trials and children at each step of screening are printed too.

Set up
------

Load the data.

``` r
library(dplyr, warn.conflicts = FALSE)
df_child <- readr::read_csv("data-raw/test-scores.csv") %>% 
  select(Group, Matching_PairNumber, 
         ChildID, ChildStudyID, Study, ResearchID, Female)
df_blocks <- readr::read_csv("data-raw/blocks.csv")
df_looks <- readr::read_csv("data-raw/looks.csv.gz")
df_trials <- readr::read_csv("data-raw/trials.csv")
```

In order to aggregate looks with my littlelisteners package, we need to define a response coding definition so it knows what's a target, what's offscreen, etc.

``` r
library(littlelisteners)
def <- create_response_def(
  label = "LWL scheme",
  primary = "Target",
  others = "Distractor",
  elsewhere = "tracked", 
  missing = NA)
```

Set data-screening options.

``` r
screening <- list(
  # determine amount of missing data between:
  min_time = 250,
  max_time = 1500,
  # remove trials with more than ... proportion of missing data
  max_na = .5,
  # blocks should have at least this many trials
  min_block = 12,
  # conditions should have at least this many trials
  min_condition = 6
)
```

Remove blocks from the bad version of the experiment
----------------------------------------------------

The first version of the experiment ran too quickly; there wasn't much time between the prompt and the reinforcer phrase.

``` r
blocks_to_drop <- df_blocks %>% 
  filter(Block_Version != "Standard") %>% 
  select(BlockID:Block_Version)

knitr::kable(blocks_to_drop)
```

|  BlockID| Study      | ResearchID |  Block\_Age| Block\_Basename       | Block\_Version         |
|--------:|:-----------|:-----------|-----------:|:----------------------|:-----------------------|
|     2056| TimePoint1 | 605L       |          31| MP\_Block1\_605L31MS1 | Early attention getter |
|     2057| TimePoint1 | 605L       |          31| MP\_Block2\_605L31MS1 | Early attention getter |
|     2061| TimePoint1 | 608L       |          39| MP\_Block1\_608L39FS2 | Early attention getter |
|     2062| TimePoint1 | 608L       |          39| MP\_Block2\_608L39FS2 | Early attention getter |

``` r

looks_good_version <- df_looks %>% 
  anti_join(blocks_to_drop)
```

Find unreliable trials
----------------------

We identify trials with more than 50% missing data during a trial window (here 250-- 1500 ms).

``` r
trial_quality <- looks_good_version %>% 
  left_join(df_child) %>% 
  # Offset by 20 ms because the data binned into 50ms bins: I.e., the frame at
  # 285ms is part of the [285, 300, 315] ms bin, so that frame needs to part of
  # the data screening.
  filter(between(Time, screening$min_time - 20, screening$max_time + 20)) 

range(trial_quality$Time)
#> [1]  233.164 1515.570

missing_data_per_trial <- trial_quality %>% 
  aggregate_looks(def, Group + Study + ResearchID + 
                    Matching_PairNumber + TrialID ~ GazeByImageAOI) %>% 
  select(Group:TrialID, PropNA) %>% 
  print()
#> # A tibble: 5,724 x 6
#>              Group      Study ResearchID Matching_PairNumber TrialID     PropNA
#>              <chr>      <chr>      <chr>               <int>   <int>      <dbl>
#>  1 CochlearImplant CochlearV1       300E                   1  104349 0.06410256
#>  2 CochlearImplant CochlearV1       300E                   1  104350 0.41025641
#>  3 CochlearImplant CochlearV1       300E                   1  104351 0.26923077
#>  4 CochlearImplant CochlearV1       300E                   1  104352 0.28205128
#>  5 CochlearImplant CochlearV1       300E                   1  104353 0.10256410
#>  6 CochlearImplant CochlearV1       300E                   1  104354 0.00000000
#>  7 CochlearImplant CochlearV1       300E                   1  104355 0.24358974
#>  8 CochlearImplant CochlearV1       300E                   1  104356 0.00000000
#>  9 CochlearImplant CochlearV1       300E                   1  104357 0.00000000
#> 10 CochlearImplant CochlearV1       300E                   1  104358 0.00000000
#> # ... with 5,714 more rows
```

Count the number of bad trials that need to be excluded.

``` r
# Using UQ to unquote `screening$max_na` so that the created column is
# *not* named `PropNA > screening$max_na` but instead uses the value of
# `screening$max_na` in the column name.
missing_data_per_trial %>% 
  count(PropNA > UQ(screening$max_na)) %>% 
  rename(`Num Trials` = n) %>% 
  knitr::kable()
```

| PropNA &gt; 0.5 |  Num Trials|
|:----------------|-----------:|
| FALSE           |        4721|
| TRUE            |        1003|

``` r

missing_data_per_trial %>% 
  count(Group,PropNA > UQ(screening$max_na)) %>%  
  rename(`Num Trials` = n) %>% 
  knitr::kable()
```

| Group           | PropNA &gt; 0.5 |  Num Trials|
|:----------------|:----------------|-----------:|
| CochlearImplant | FALSE           |        2131|
| CochlearImplant | TRUE            |         641|
| NormalHearing   | FALSE           |        2590|
| NormalHearing   | TRUE            |         362|

Remove the bad trials.

``` r
bad_trials <- missing_data_per_trial %>% 
  filter(PropNA >= screening$max_na)

looks_clean_trials <- anti_join(looks_good_version, bad_trials)
```

Find unreliable blocks
----------------------

Count the number of trials leftover in each block.

``` r
# Narrow down to one row per trial
trial_counts <- looks_clean_trials %>% 
  select(BlockID:ResearchID) %>% 
  distinct() %>% 
  # Count rows in each group
  count(BlockID, Study, ResearchID) %>% 
  arrange(n) %>% 
  rename(`Num Trials in Block` = n)
```

Identify blocks with fewer than 12 trials.

``` r
blocks_to_drop <- trial_counts %>% 
  filter(`Num Trials in Block` < screening$min_block) 

knitr::kable(blocks_to_drop)
```

|  BlockID| Study      | ResearchID |  Num Trials in Block|
|--------:|:-----------|:-----------|--------------------:|
|     2804| TimePoint3 | 605L       |                    2|
|     2454| TimePoint2 | 605L       |                    3|
|     2877| TimePoint3 | 665L       |                    5|
|     2803| TimePoint3 | 605L       |                    8|
|     2182| TimePoint1 | 679L       |                   11|
|     2532| TimePoint2 | 665L       |                   11|

Remove the blocks.

``` r
looks_clean_blocks <- looks_clean_trials %>% 
  anti_join(blocks_to_drop) 
```

Enforce minimum number of trials per condition
----------------------------------------------

Count the number of trials leftover in each condition.

``` r
# Narrow down to one row per trial
condition_counts <- looks_clean_blocks %>% 
  left_join(df_trials %>% select(TrialID, Condition)) %>% 
  select(Study, ResearchID, TrialID, Condition) %>% 
  distinct() %>% 
  # Count rows in each group
  count(Condition, Study, ResearchID) %>% 
  # Make sure 0 trials get counted
  tidyr::complete(tidyr::nesting(Study, ResearchID), Condition, 
                  fill = list(n = 0)) %>% 
  arrange(n) %>% 
  rename(`Num Trials in Condition` = n) %>% 
  print()
#> # A tibble: 234 x 4
#>         Study ResearchID Condition `Num Trials in Condition`
#>         <chr>      <chr>     <chr>                     <dbl>
#>  1 TimePoint2       665L        MP                         6
#>  2 TimePoint3       665L  nonsense                         6
#>  3 CochlearV2       312E  nonsense                         8
#>  4 TimePoint2       665L  nonsense                         8
#>  5 TimePoint3       665L        MP                         8
#>  6 TimePoint3       665L      real                         8
#>  7 TimePoint2       679L  nonsense                         9
#>  8 TimePoint1       679L      real                        10
#>  9 TimePoint2       665L      real                        10
#> 10 CochlearV1       306E      real                        11
#> # ... with 224 more rows
```

We want to ensure that there are at least 6 trials per condition.

``` r
children_to_drop <- condition_counts %>% 
  filter(`Num Trials in Condition` < screening$min_condition) 

if (nrow(children_to_drop) == 0) {
  print("No children to remove")
} else {
  knitr::kable(children_to_drop)
}
#> [1] "No children to remove"
```

Remove the children.

``` r
looks_clean_conditions <- looks_clean_blocks %>% 
  anti_join(children_to_drop) 
```

Remove unpaired children
------------------------

Next, we need to exclude participants who no longer have a match.

``` r
df_leftover <- looks_clean_conditions %>% 
  distinct(Study, ResearchID) %>% 
  left_join(df_child)

df_leftover %>% 
  count(Group) %>% 
  ungroup() %>% 
  rename(`Num Children` = n)
#> # A tibble: 2 x 2
#>             Group `Num Children`
#>             <chr>          <int>
#> 1 CochlearImplant             37
#> 2   NormalHearing             41

df_singletons <- df_leftover %>% 
  count(Matching_PairNumber) %>% 
  ungroup() %>% 
  rename(NumChildrenInPair = n) %>% 
  filter(NumChildrenInPair == 1) %>% 
  print()
#> # A tibble: 4 x 2
#>   Matching_PairNumber NumChildrenInPair
#>                 <int>             <int>
#> 1                  21                 1
#> 2                  22                 1
#> 3                  23                 1
#> 4                  24                 1

df_looks_clean <- looks_clean_conditions %>% 
  left_join(df_child %>% 
               select(Group, Matching_PairNumber, Study, ResearchID)) %>% 
  anti_join(df_singletons)
```

Now, there will be the same number of children in each group x task.

``` r
df_looks_clean %>% 
  distinct(Group, Study, ResearchID) %>% 
  count(Group) %>% 
  ungroup() %>% 
  rename(`Num Children` = n) %>% 
  knitr::kable()
```

| Group           |  Num Children|
|:----------------|-------------:|
| CochlearImplant |            37|
| NormalHearing   |            37|

``` r

# Make sure there are 2 children in every matching pair
df_looks_clean %>% 
  distinct(Matching_PairNumber, Group, Study, ResearchID) %>% 
  count(Matching_PairNumber) %>% 
  ungroup() %>% 
  rename(NumChildrenInPair = n) %>% 
  filter(NumChildrenInPair != 2)
#> # A tibble: 0 x 2
#> # ... with 2 variables: Matching_PairNumber <int>, NumChildrenInPair <int>
```

Data screening counts
---------------------

Count the number of children and trials at each stage in data screening.

``` r
cleaning_progression <- list(
  `a. raw data` = df_looks,
  `b. remove bad experiment version` = looks_good_version,
  `c. drop bad trials` = looks_clean_trials, 
  `d. drop sparse blocks` = looks_clean_blocks,
  `e. drop children w sparse conditions` = looks_clean_conditions,
  `f. drop unpaired children` = df_looks_clean) %>% 
  bind_rows(.id = "Stage") %>% 
  select(-Group, -Matching_PairNumber) %>% 
  left_join(df_child) 

cleaning_progression %>% 
  group_by(Stage) %>% 
  summarise(
    `Num Pairs` = n_distinct(Matching_PairNumber),
    `Num Children` = n_distinct(ChildID),
    `Num Child-Study IDs` = n_distinct(ChildStudyID),
    `Num Blocks` = n_distinct(BlockID),
    `Num Trials` = n_distinct(TrialID)) %>% 
  knitr::kable()
```

| Stage                                |  Num Pairs|  Num Children|  Num Child-Study IDs|  Num Blocks|  Num Trials|
|:-------------------------------------|----------:|-------------:|--------------------:|-----------:|-----------:|
| a. raw data                          |         41|            52|                   82|         163|        5868|
| b. remove bad experiment version     |         41|            52|                   80|         159|        5724|
| c. drop bad trials                   |         41|            52|                   80|         159|        4703|
| d. drop sparse blocks                |         41|            51|                   78|         153|        4663|
| e. drop children w sparse conditions |         41|            51|                   78|         153|        4663|
| f. drop unpaired children            |         37|            50|                   74|         145|        4394|

``` r

cleaning_progression %>% 
  group_by(Stage, Group) %>% 
  summarise(
    `Num Children` = n_distinct(ChildID),
    `Num Child-Study IDs` = n_distinct(ChildStudyID),
    `Num Blocks` = n_distinct(BlockID),
    `Num Trials` = n_distinct(TrialID)) %>% 
  knitr::kable()
```

| Stage                                | Group           |  Num Children|  Num Child-Study IDs|  Num Blocks|  Num Trials|
|:-------------------------------------|:----------------|-------------:|--------------------:|-----------:|-----------:|
| a. raw data                          | CochlearImplant |            26|                   41|          81|        2916|
| a. raw data                          | NormalHearing   |            26|                   41|          82|        2952|
| b. remove bad experiment version     | CochlearImplant |            26|                   39|          77|        2772|
| b. remove bad experiment version     | NormalHearing   |            26|                   41|          82|        2952|
| c. drop bad trials                   | CochlearImplant |            26|                   39|          77|        2121|
| c. drop bad trials                   | NormalHearing   |            26|                   41|          82|        2582|
| d. drop sparse blocks                | CochlearImplant |            25|                   37|          71|        2081|
| d. drop sparse blocks                | NormalHearing   |            26|                   41|          82|        2582|
| e. drop children w sparse conditions | CochlearImplant |            25|                   37|          71|        2081|
| e. drop children w sparse conditions | NormalHearing   |            26|                   41|          82|        2582|
| f. drop unpaired children            | CochlearImplant |            25|                   37|          71|        2081|
| f. drop unpaired children            | NormalHearing   |            25|                   37|          74|        2313|

Clean up
--------

``` r
df_looks_w_info <- df_looks_clean %>% 
  left_join(df_blocks) %>% 
  left_join(df_trials) %>% 
  left_join(df_child) %>% 
  select(ChildID, ChildStudyID, Group, Matching_PairNumber, Study, ResearchID, 
         Block_Basename, Block_Dialect, StimulusSet, Block_Version,
         TrialNo, Condition:TargetImage, Time:GazeByAOI)

df_looks_w_info %>% 
  distinct(Block_Dialect, StimulusSet, Block_Version)
#> # A tibble: 2 x 3
#>   Block_Dialect StimulusSet Block_Version
#>           <chr>       <chr>         <chr>
#> 1           SAE         TP1      Standard
#> 2           SAE         TP2      Standard

df_looks_w_info <- df_looks_w_info %>% 
  select(-Block_Dialect, -Block_Version, -StimulusSet)
```

Save clean data.

``` r
df_looks_w_info %>% 
  mutate(
     Group_Lab = Group %>% 
      factor(c("CochlearImplant", "NormalHearing"), 
             c("Cochlear implant", "Normal hearing"))) %>% 
  readr::write_csv("./data/screened.csv.gz")
```

Update participants data to only have children with eyetracking data.

``` r
readr::read_csv(file.path(".", "data-raw", "test-scores.csv")) %>% 
  semi_join(df_looks_w_info) %>% 
  mutate(
    Group_Lab = Group %>% 
      factor(c("CochlearImplant", "NormalHearing"), 
             c("Cochlear implant", "Normal hearing"))) %>%
  readr::write_csv("./data/scores.csv")
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
#>  package         * version    date       source                                 
#>  assertthat        0.2.0      2017-04-11 CRAN (R 3.3.2)                         
#>  backports         1.1.1      2017-09-25 CRAN (R 3.4.1)                         
#>  bindr             0.1        2016-11-13 CRAN (R 3.4.0)                         
#>  bindrcpp        * 0.2        2017-06-17 CRAN (R 3.4.0)                         
#>  clisymbols        1.2.0      2017-08-04 Github (gaborcsardi/clisymbols@e49b4f5)
#>  digest            0.6.12     2017-01-27 CRAN (R 3.3.2)                         
#>  dplyr           * 0.7.4      2017-09-28 CRAN (R 3.4.2)                         
#>  evaluate          0.10.1     2017-06-24 CRAN (R 3.4.1)                         
#>  glue              1.2.0      2017-10-29 CRAN (R 3.4.2)                         
#>  highr             0.6        2016-05-09 CRAN (R 3.2.3)                         
#>  hms               0.3        2016-11-22 CRAN (R 3.3.2)                         
#>  htmltools         0.3.6      2017-04-28 CRAN (R 3.4.0)                         
#>  knitr           * 1.17       2017-08-10 CRAN (R 3.4.2)                         
#>  littlelisteners * 0.0.0.9000 2017-09-22 Github (tjmahr/littlelisteners@44e87a4)
#>  magrittr          1.5        2014-11-22 CRAN (R 3.1.2)                         
#>  pkgconfig         2.0.1      2017-03-21 CRAN (R 3.3.3)                         
#>  purrr             0.2.4      2017-10-18 CRAN (R 3.4.2)                         
#>  R6                2.2.2      2017-06-17 CRAN (R 3.4.0)                         
#>  Rcpp              0.12.13    2017-09-28 CRAN (R 3.4.2)                         
#>  readr             1.1.1      2017-05-16 CRAN (R 3.4.0)                         
#>  rlang             0.1.4      2017-11-05 CRAN (R 3.4.2)                         
#>  rmarkdown         1.6        2017-06-15 CRAN (R 3.4.2)                         
#>  rprojroot         1.2        2017-01-16 CRAN (R 3.3.2)                         
#>  sessioninfo       1.0.1      2017-09-13 Github (r-lib/sessioninfo@e813de4)     
#>  stringi           1.1.5      2017-04-07 CRAN (R 3.3.3)                         
#>  stringr           1.2.0      2017-02-18 CRAN (R 3.3.2)                         
#>  tibble            1.3.4      2017-08-22 CRAN (R 3.4.1)                         
#>  tidyr             0.7.2      2017-10-16 CRAN (R 3.4.2)                         
#>  tidyselect        0.2.3      2017-11-06 CRAN (R 3.4.2)                         
#>  withr             2.1.0.9000 2017-11-02 Github (jimhester/withr@8ba5e46)       
#>  yaml              2.1.14     2016-11-12 CRAN (R 3.4.2)
```
