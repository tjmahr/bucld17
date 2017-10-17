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


## Prepare eyetracking database tables

# This is a tedious step. We need to download the eyetracking data. First, let's
# connect to the database and prepare some queries.

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


## Download the database tables

df_blocks <- collect(tbl_blocks) %>%
  left_join(df_child_info) %>%
  group_by(ChildStudyID) %>%
  # We want one age per child, so use earliest. This might be dubious.
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
