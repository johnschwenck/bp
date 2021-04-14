## Adjustment File for Raw Pregnancies Data File from Dryad

bp_preg <- read.csv('data-raw/bp_preg/bp_preg.csv')

# Remove empty columns
bp_preg <- bp_preg[ , colSums(is.na(bp_preg)) < nrow(bp_preg)]

# Rename Booking BP reading columns
colnames(bp_preg)[55] <- "SBP_Booking"
colnames(bp_preg)[56] <- "DBP_Booking"

# Create ID Column
bp_preg$ID <- seq.int(nrow(bp_preg))

# Pivot timed BP columns for SBP and DBP
bp_preg_adj <- bp_preg %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("SBP_")|dplyr::starts_with("DBP_"),
    names_to = c(".value", "Time_Elapsed"),
    names_sep = "_"
  ) %>%
  relocate(ID, Time_Elapsed, SBP, DBP) %>%
  relocate(Privacy, AN_PET, IP_PET, PN_PET, PIERS, .after = EDC_Dryad)

usethis::use_data(bp_preg_adj, overwrite = TRUE)











data.frame(id = 1:10,
           sbp_0 = round(rnorm(10, 120, 10), 0),
           sbp_30 = round(rnorm(10, 120, 10), 0),
           sbp_60 = round(rnorm(10, 120, 10), 0),
           dbp_0 = round(rnorm(10, 80, 5), 0),
           dbp_30 = round(rnorm(10, 80, 5), 0),
           dbp_60 = round(rnorm(10, 80, 5), 0))

data.frame(id = rep(1:10, each = 3),
           time_elapsed = c(0, 30, 60),
           sbp = round(rnorm(30, 120, 10), 0),
           dbp = round(rnorm(30, 80, 5), 0)
)
