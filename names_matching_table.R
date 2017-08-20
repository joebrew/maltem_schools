library(tidyverse)
library(gsheet)

# Get original and post-manual cleaning names from google docs
names_a <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1CgF122GCSkkIJWjwEyXtRZTm4p_PyXZXf_LT1Bq3k4Y/edit?usp=sharing')

# Get the algorithm (openrefine) only cleaned names
or <- read_csv('data/open_refine_results.csv')

# Change column names for clarity
or <-
  or %>%
  rename(name_cleaned_by_open_refine = new_name)
names_a <- 
  names_a %>%
  dplyr::select(school, name, new_name) %>%
  rename(name_cleaned_by_open_refine_then_manually = new_name)

# Join the two datasets
names_matching <-
  left_join(x = or,
            y = names_a,
            by = c('school', 'name'))

# Define what kinds of changes
names_matching$no_changes <- 
  names_matching$name == names_matching$name_cleaned_by_open_refine_then_manually
names_matching$algorithmic_changes <-
  names_matching$name != names_matching$name_cleaned_by_open_refine
names_matching$manual_changes_only <-
  (names_matching$name == names_matching$name_cleaned_by_open_refine) &
  (names_matching$name != names_matching$name_cleaned_by_open_refine_then_manually)
names_matching$algorithmic_changes_only <-
  names_matching$algorithmic_changes &
  names_matching$name_cleaned_by_open_refine == 
  names_matching$name_cleaned_by_open_refine_then_manually
names_matching$algorithm_overcorrected <-
  names_matching$algorithmic_changes &
  (names_matching$name_cleaned_by_open_refine != 
     names_matching$name_cleaned_by_open_refine_then_manually) &
  names_matching$name_cleaned_by_open_refine_then_manually == 
  names_matching$name
names_matching$algorithmic_and_manual_changes <-
  !names_matching$algorithm_overcorrected &
  names_matching$algorithmic_changes &
  names_matching$name_cleaned_by_open_refine_then_manually !=
  names_matching$name_cleaned_by_open_refine


# Save to outputs
write_csv(names_matching, 'outputs/names_matching.csv')

# Summary text
denom <- nrow(names_matching)
with_p <- function(val){
  paste0(val, ' (',
         round(val / denom * 100, digits = 2),
         ' percent) ')
}
summary_text <-
  paste0('There were ', denom, ' distinct names in the original datasets.\n',
         with_p(length(which(names_matching$no_changes))),
         'of these names underwent 0 changes.\n',
         with_p(length(which(names_matching$algorithmic_changes))),
         ' of these names underwent algorithmic changes.\n',
         with_p(length(which(names_matching$manual_changes_only))),
         ' of these names underwent manual changes only.\n',
         'In the case of ',
         length(which(names_matching$algorithm_overcorrected)),
         ' the algorithm over-corrected (ie, the algorithm changed the name, ',
         'but then the name was changed back to the original one in manual review.\n',
         with_p(length(which(names_matching$manual_changes_only))),
         ' names underwent only manual changes (ie, the algorithm made no changes, but the manual process did make changes.)\n',
         length(which(names_matching$algorithmic_and_manual_changes)),
         ' (not including the "over-correction" names) underwent both algorithmic and manual changes.')

cat(paste0(summary_text))
