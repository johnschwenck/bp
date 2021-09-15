############################################################
#                                                          #
#   bp: Blood Pressure Analysis in R -  Code  for Figures #
#        Authors: John Schwenck & Irina Gaynanova          #
#                                                          #
#       Plots, Figures, and Output for JSS Article         #
#                                                          #
############################################################

# Load bp Package
library(bp)

### Section 3: Data Processing

# Names of JHS data set
names(bp_jhs)

# Names of HYPNOS data set
names(bp_hypnos)

# Process JHS data set
jhs_proc <- process_data(bp_jhs,
                         bp_type = "HBPM",
                         sbp = "Sys.mmHg.",
                         dbp = "Dias.mmHg.",
                         date_time = "DateTime",
                         hr = "pulse.bpm.")

# Process HYPNOS data set
hypnos_proc <- process_data(bp_hypnos,
                            bp_type = "abpm",
                            sbp = "syst",
                            dbp = "diast",
                            date_time = "date.time",
                            hr = "hr",
                            pp = "PP",
                            map = "MaP",
                            rpp = "Rpp",
                            id = "id",
                            visit = "Visit",
                            wake = "wake")

# Names of processed JHS data
names(jhs_proc)

# Names of processed HYPNOS data
names(hypnos_proc)


### Section 4: Metrics and Visualizations

# Figure 2: Sleep Periods
# NOTE: Sleep period labels on figure in paper were added with photo editing software
fig2_data <- hypnos_proc %>% dplyr::filter(ID == "70435" & VISIT == 1)
fig2_data$HOUR <- factor(fig2_data$HOUR, levels = c(12:23, 0:11))
ggplot2::ggplot(fig2_data, ggplot2::aes(x = HOUR, y = SBP)) +
  ggplot2::geom_point(ggplot2::aes(color = WAKE)) +
  ggplot2::ggtitle("BP Sleep Periods", subtitle = "HYPNOS Subject: 70435") +
  ggplot2::annotate("rect", xmin = '23', xmax = '6', ymin = -Inf, ymax = Inf, fill = 'gold', alpha = .45) +
  ggplot2::theme_bw()



### Section 5: Case Study I - JHS Data


## Section 5.1: Data Aggregation

# Display a few rows of selected columns without averaging
head(jhs_proc[4:14, c(1:8, 13)], 10)

# Re-process data but aggregate values
jhs_proc_agg <- process_data(bp_jhs,
                               sbp = "Sys.mmHg.",
                               dbp = "Dias.mmHg.",
                               date_time = "DateTime",
                               hr = "pulse.bpm.",
                               agg = TRUE)

# Display a few rows of selected columns with averaging
head(jhs_proc_agg[4:14, c(1:6, 8:9, 13:14)], 10)

# Re-process data but aggregate AND collapse values
jhs_proc_agg_collapsed <- process_data(bp_jhs,
                                       sbp = "Sys.mmHg.",
                                       dbp = "Dias.mmHg.",
                                       date_time = "DateTime",
                                       hr = "pulse.bpm.",
                                       agg = TRUE,
                                       collapse_df = TRUE)

# Display a few rows of selected columns with averaging AND collapse the data
head(jhs_proc_agg_collapsed[3:14, c(1:6, 8:9, 13:14)], 6)


## Section 5.2: End-of-Day Determination

# Set an eod cutoff of 4 AM (eod = 0400)
jhs_proc_eod <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.",
                             eod = "0400")

# Display the data with Dates adjusted for this cutoff
head(jhs_proc_eod[9:14, c(1:8, 13)], 15)


## Section 5.3: Generating a Report

# Put it all together in a new process data call
jhs_proc_report <- process_data(bp_jhs,
                                sbp = "Sys.mmHg.",
                                dbp = "Dias.mmHg.",
                                date_time = "DateTime",
                                hr = "pulse.bpm.",
                                eod = "0400",
                                agg = TRUE,
                                collapse_df = TRUE)

# Display the report
bp_report(jhs_proc_report, group_var = "TIME_OF_DAY", save_report = FALSE)



### Section 6: Case Study II - HYPNOS Data


## Section 6.1 - Time Series Plots

# Time series plots for subjects 70435 and 70439
out <- bp_ts_plots(hypnos_proc,first_hour = 11,wrap_var ='visit',subj = c('70435','70439') )


## Section 6.2 - Nocturnal Dipping Calculation

# Dip calc for subject 70435
dip_calc(hypnos_proc, subj = '70435')

# Dip calc for subject 70439
dip_calc(hypnos_proc, subj = '70439')

# Identify the outlier value
hypnos_proc %>%
  dplyr::filter(ID =='70439') %>%
  dplyr::arrange(desc(SBP)) %>%
  head(3) %>%
  dplyr::select(SBP, DBP, BP_CLASS, DATE_TIME, MAP, HR, RPP, WAKE, VISIT)

# Re-calculated HYPNOS data after screening out the outlier value
hypnos_proc_recalc <- process_data(bp_hypnos,
                                   bp_type = "abpm",
                                   sbp = "syst",
                                   dbp = "diast",
                                   date_time = "date.time",
                                   hr = "hr",
                                   pp = "PP",
                                   map = "MaP",
                                   rpp = "Rpp",
                                   id = "id",
                                   visit = "Visit",
                                   wake = "wake",
                                   DUL = 130)

# Run dip calc again but with filtered HYPNOS data
dip_calc(hypnos_proc_recalc, subj = c('70439') )

# Dipping category plots before and after removing outlier
dip_class_plot(hypnos_proc, subj = c('70435','70439'))
dip_class_plot(hypnos_proc_recalc, subj = c('70435','70439'))


## Section 6.3 - Sleep-period Metrics
bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))

# SBP-related sleep metrics
bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))[[4]][,c(1:10)]

# DBP-related sleep metrics
bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))[[4]][,c(1:3, 11:17)]


# Section 6.4 - Weighted Standard Deviation (wSD)

# Combine Regular SD with wSD for original data
cbind(
  bp_sleep_metrics(hypnos_proc, subj = c('70439'))[[4]][,c(1:3, 10, 17)],
  bp_stats(hypnos_proc, subj = c('70439'), inc_wake = F)[,c(8:9)]
  )

# Combine Regular SD with wSD for re-calculated data with outlier omitted
cbind(
  bp_sleep_metrics(hypnos_proc_recalc, subj = c('70439'))[[4]][,c(1:3, 10, 17)],
  bp_stats(hypnos_proc, subj = c('70439'), inc_wake = F)[,c(8:9)]
)


