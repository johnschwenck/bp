# Calculate the drop in BP during sleep to determine dipping pattern

# To Do: - If Awake indicator column is specified in dataset, calculate percentages according to indicator.
#              If not, default to Awake: 6am - 11pm | Asleep: 11pm - 6am |
#        - Give user ability to choose time frame interval if no awake indicator column provided
#        - User-defined dipping threshold, default to 10%
#        - Screening criteria  for {SBP > 250 | SBP < 70} and {DBP < 45 | DBP > 150} and {HR < 40 | HR > 200} according to Holt-Lunstad, Jones, and Birmingham (2009) paper


# Calculate the percent difference between two successive groups. In this case: Awake vs Asleep
pct = function(X_vec) {
  return(X_vec[1]/X_vec[2])
}

dip <- data %>%
  group_by(ID, VISIT, WAKE) %>%
  summarise(Avg_BP = mean(SBP))

dip_thresh <- 0.10
dip_pct <- dip %>%
  summarise(dip = -(1 - pct(Avg_BP)) ) %>%
  mutate(out = ifelse(dip <= -dip_thresh, "dipper",
                      ifelse(dip > 0, "reverse", "non-dipper")))


dip_pct

