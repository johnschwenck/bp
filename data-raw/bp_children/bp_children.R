## Adjustment File for Raw B-Proact1v Child BP Study Data File
## Contact: Dr. Russ Jago - Bristol (UK)

library(haven)
library(dplyr)
library(Hmisc)

# Use Haven package to read in DTA file
bp_children <- haven::read_dta('data-raw/bp_children/bp_children.dta')

# Columns to remove
rm_cols <- c("school_id",
             "c_zbmi_2",
             "c_zbmi_3",
             "p1_high_bp_2",
             "p1_high_bp_3")

# Remove indicated columns
bp_children <- bp_children[, -which(names(bp_children) %in% rm_cols)]

# Re-order HH Edu column
bp_children <- bp_children %>% dplyr::relocate(hh_educ, .after = c_gender)

# Remove "c_" prefix
colnames(bp_children) <- gsub("c_", "", colnames(bp_children) )

# Temp
colnames(bp_children)[c(24, 25, 26, 27, 28, 29, 30, 31)] <- c("N.valid.days.all_2", "N.valid.days.all_3", "avg.mins.all_2",
                                                              "sed.avg.mins.all_2", "mvpa.avg.mins.all_2", "avg.mins.all_3",
                                                              "sed.avg.mins.all_3",  "mvpa.avg.mins.all_3")

# Pivot (1st time) to split according to visit
bp_children_adj <- tidyr::pivot_longer(bp_children,
                    cols = dplyr::ends_with("_2") | dplyr::ends_with("_3"),
                    names_to = c(".value","visit"),
                    names_sep = "_"
                    )

# Change visit from {2,3} to {1,2}
bp_children_adj$visit <- ifelse(bp_children_adj$visit == 2, 1, bp_children_adj$visit)
bp_children_adj$visit <- ifelse(bp_children_adj$visit == 3, 2, bp_children_adj$visit)

# Pivot (2nd time) to split SBP/DBP
bp_children_adj <- tidyr::pivot_longer(bp_children_adj,
                           cols = dplyr::starts_with("sbp") | dplyr::starts_with("dbp"),
                           names_to = c(".value","reading"),
                           names_sep = "bp",
                           values_drop_na = TRUE
) %>%
  dplyr::rename(sbp = s, dbp = d) %>%
  dplyr::relocate(reading, sbp, dbp, .after = id)

# Convert from meters & kilograms to ft & pounds
bp_children_adj$ht <- bp_children_adj$ht * 3.23084
bp_children_adj$wt <- bp_children_adj$wt * 2.20462

# Add variable labels
var.labels <- c(  id = "Child ID",
                  gender = "Gender",
                  hh_educ = "Household Highest Education",
                  visit = "Visit #",
                  age = "Age",
                  ht = "Height (ft)",
                  wt = "Weight (lbs)",
                  bmi = "Body Mass Index",
                  reading = "BP Reading #",
                  sbp = "Systolic Blood Pressure",
                  dbp = "Diastolic Blood Pressure",
                  N.valid.days.all = "Number of days of accelerometer data",
                  avg.mins.all = "Avg # minutes per day of activity",
                  sed.avg.mins.all = "Avg # minutes of sedentary activity",
                  mvpa.avg.mins.all = "Avg # minutes of Moderate to Vigorous Physical Activity"
                )

# Add labels to df
Hmisc::label(bp_children_adj) <- as.list(var.labels[match(names(bp_children_adj), names(var.labels))])
#labelled::remove_val_labels(data) # To remove labels

#View(bp_children_adj)
bp_children <- bp_children_adj
rm(bp_children_adj)

usethis::use_data(bp_children, overwrite = TRUE)
