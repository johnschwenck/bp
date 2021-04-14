## Adjustment File for Raw Ghana Study Data File from Dryad

library(dplyr)

# NOTE: Data combined in Excel and converted to CSV

bp_ghana <- read.csv('data-raw/bp_ghana/bp_ghana_raw.csv')

colnames(bp_ghana) <- gsub("_", "", colnames(bp_ghana) )
colnames(bp_ghana) <- gsub("SBP", "SBP_", colnames(bp_ghana))
colnames(bp_ghana) <- gsub("DBP", "DBP_", colnames(bp_ghana))
colnames(bp_ghana)[1] <- "ID"


bp_ghana_adj <- bp_ghana %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("SBP")|dplyr::starts_with("DBP"),
    names_to = c(".value", "Time_Elapsed"),
    names_sep = "_"
  )%>%
  relocate(ID, Time_Elapsed, SBP, DBP)

#View(bp_ghana_adj)
bp_ghana <- bp_ghana_adj
rm(bp_ghana_adj)

usethis::use_data(bp_ghana, overwrite = TRUE)
