############################################################
#                                                          #
#   bp: Blood Pressure Analysis in R -  Code  for Figures  #
#        Authors: John Schwenck & Irina Gaynanova          #
#                                                          #
#       Plots, Figures, and Output for the Article         #
#                  Replication Script                      #
#                                                          #
############################################################


# Install / load bp and other necessary packages
list.of.packages <- c("bp", "ggplot2", "patchwork", "ggforce", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(bp)
library(ggplot2)
library(patchwork)
library(ggforce)
library(dplyr)

# Path to save output (default set to "paper/" to reflect path within package). Change this to desired folder location
path <- "paper/"

### Section 3: Data Processing ##################

# Names of JHS data set
names(bp_jhs)

# Names of HYPNOS data set
names(bp_hypnos)

# Process JHS data set
jhs_proc <- process_data(bp_jhs, bp_type = "hbpm",
                                     sbp = "Sys.mmHg.",
                                     dbp = "Dias.mmHg.",
                               date_time = "DateTime",
                                      hr = "Pulse.bpm.")

# Process HYPNOS data set
hypnos_proc <- process_data(bp_hypnos, bp_type = "abpm",
                                           sbp = "SYST",
                                           dbp = "DIAST",
                                     date_time = "DATE.TIME",
                                            hr = "HR",
                                            pp = "PP",
                                           map = "MAP",
                                           rpp = "RPP",
                                            id = "ID",
                                         visit = "VISIT",
                                          wake = "WAKE")


# Names of processed JHS data
names(jhs_proc)

# Names of processed HYPNOS data
names(hypnos_proc)


##### Section 4: Metrics and Visualizations ##########

# Section 4.1

# Figure 1: Scatter plot of blood pressure measurements
p1 = bp_scatter(hypnos_proc, subj = '70435',
                        group_var = "VISIT",
                         wrap_var = "TIME_OF_DAY")

pdf(file = paste(path,"figure1_scatterplot.pdf", sep = ""), width = 11, height = 8)
print(p1)
dev.off()

# Section 4.2

# ARV Calculation
bp_arv(jhs_proc)

# Figure 2: Sleep Periods identification

hypnos_proc$WAKE = as.factor(hypnos_proc$WAKE)

fig2_data <- hypnos_proc %>% dplyr::filter(ID == "70435" & VISIT == 1)
fig2_data$HOUR <- factor(fig2_data$HOUR, levels = c(12:23, 0:11))
p2 = ggplot2::ggplot(fig2_data, ggplot2::aes(x = HOUR, y = SBP)) +
  ggplot2::geom_point(ggplot2::aes(color = WAKE), size = 3) +
  ggplot2::ggtitle("BP Sleep Periods", subtitle = "HYPNOS Subject: 70435") +
  ggplot2::annotate("rect", xmin = 11.5, xmax = 17.5, ymin = -Inf, ymax = Inf, fill = 'gold', alpha = .45) +
  ggplot2::theme_bw() + ylim(c(80, 160))

p2 = p2 +
  geom_text(aes(x = 4, y = 158, label = 'Wake'), color = 'black', hjust = 0, vjust = 0, size = 8, check_overlap = TRUE) +
  geom_text(aes(x = 19, y = 158, label = 'Wake'), color = 'black', hjust = 0, vjust = 0, size = 8, check_overlap = TRUE) +
  geom_text(aes(x = 13, y = 158, label = 'Nocturnal'), color = 'black', hjust = 0, vjust = 0, size = 8, check_overlap = TRUE)

p2 = p2 +
  annotate("rect", xmin = 9.5,
           xmax = 11.5,
           ymin = 125,
           ymax = 150,
           fill = 'red3', alpha = .2) +
  annotate("rect", xmin = 17.5,
           xmax = 19.5,
           ymin = 100,
           ymax = 140,
           fill = 'red3', alpha = .2) +
  annotate("rect", xmin = 14.5,
           xmax = 17.5,
           ymin = 80,
           ymax = 135,
           fill = 'red3', alpha = .2) +
  annotate("rect", xmin = 14.8,
           xmax = 17.2,
           ymin = 87,
           ymax = 123,
           fill = 'brown', alpha = .2) +
  geom_text(aes(x = 9.5, y = 151, label = 'Presleep BP'), color = 'black', hjust = 0, vjust = 0, size = 4, check_overlap = TRUE) +
  geom_text(aes(x = 15, y = 124, label = 'Lowest BP'), color = 'black', hjust = 0, vjust = 0, size = 4, check_overlap = TRUE) +
  geom_text(aes(x = 15, y = 136, label = 'Prewake BP'), color = 'black', hjust = 0, vjust = 0, size = 4, check_overlap = TRUE) +
  geom_text(aes(x = 17.5, y = 141, label = 'Postwake BP'), color = 'black', hjust = 0, vjust = 0, size = 4, check_overlap = TRUE)

pdf(file = paste(path,"figure2_sleep_periods.pdf", sep = ""), width = 11, height = 5)
print(p2)
dev.off()



### Section 5: Case Study I - JHS Data ################


## Section 5.1: Data Aggregation

# Repeated from Section 3 for comparison purposes of function arguments
# jhs_proc <- process_data(bp_jhs,
#                          bp_type = "hbpm",
#                          sbp = "Sys.mmHg.",
#                          dbp = "Dias.mmHg.",
#                          date_time = "DateTime",
#                          hr = "Pulse.bpm.")

# Display a few rows of selected columns without averaging
jhs_proc[4:14, c("SBP", "DBP", "BP_CLASS", "ID", "GROUP", "DATE_TIME", "DATE", "DAY_OF_WEEK", "HOUR", "TIME_OF_DAY")]

# Re-process data but aggregate values
jhs_proc_agg <- process_data(bp_jhs, bp_type = "hbpm",
                                         sbp = "Sys.mmHg.",
                                         dbp = "Dias.mmHg.",
                                   date_time = "DateTime",
                                          hr = "Pulse.bpm.",
                                         agg = TRUE)

# Display averaged ones
jhs_proc_agg[4:14, c("SBP", "DBP", "BP_CLASS", "ID", "GROUP", "DATE_TIME", "DATE", "DAY_OF_WEEK", "HOUR", "TIME_OF_DAY")]

# Re-process data but aggregate AND collapse values
jhs_proc_agg_collapsed <- process_data(bp_jhs, bp_type = "hbpm",
                                                   sbp = "Sys.mmHg.",
                                                   dbp = "Dias.mmHg.",
                                             date_time = "DateTime",
                                                    hr = "Pulse.bpm.",
                                                   agg = TRUE,
                                           collapse_df = TRUE)

# Display a few rows of selected columns with averaging AND collapse the data
jhs_proc_agg_collapsed[3:9, c("SBP", "DBP", "BP_CLASS", "ID", "GROUP", "DATE_TIME", "DATE", "DAY_OF_WEEK", "HOUR", "TIME_OF_DAY")]


## Section 5.2: End-of-Day Determination

# Set an eod cutoff of 4 AM (eod = 0400)
jhs_proc_eod <- process_data(bp_jhs, bp_type = 'hbpm',
                                         sbp = "Sys.mmHg.",
                                         dbp = "Dias.mmHg.",
                                   date_time = "DateTime",
                                          hr = "Pulse.bpm.",
                                         eod = "0600")

# Display the data with Dates adjusted for this cutoff
jhs_proc_eod[4:14, c("SBP", "DBP", "BP_CLASS", "ID", "GROUP", "DATE_TIME", "DATE", "DAY_OF_WEEK", "HOUR", "TIME_OF_DAY")]

## Section 5.3: Generating a Report

# Figure 3: bp_report
# Put it all together in a new process data call
jhs_proc_report <- process_data(bp_jhs, bp_type = 'hbpm',
                                            sbp = "Sys.mmHg.",
                                            dbp = "Dias.mmHg.",
                                      date_time = "DateTime",
                                             hr = "Pulse.bpm.",
                                            eod = "0600",
                                            agg = TRUE,
                                    collapse_df = TRUE)

# Display the report
out = bp_report(jhs_proc_report, group_var = "TIME_OF_DAY", save_report = FALSE)

pdf(file = paste(path,"figure3_report.pdf", sep = ""), width = 14, height = 9)
gridExtra::grid.arrange(out)
dev.off()


### Section 6: Case Study II - HYPNOS Data ################


## Section 6.1 - Time Series Plots

# Figure 4: time series plots
# Time series plots for subjects 70435 and 70439
out <- bp_ts_plots(hypnos_proc, first_hour = 11,
                                  wrap_var = 'visit',
                                      subj = c('70435', '70439') )

pdf(file = paste(path,"figure4_ts_plots_dt.pdf", sep = ""), width = 14, height = 4)
out[[1]][[1]] + out[[1]][[2]]
dev.off()

pdf(file = paste(path,"figure4_ts_plots_hour.pdf", sep = ""), width = 14, height = 4)
out[[2]][[1]] + out[[2]][[2]]
dev.off()

## Section 6.2 - Nocturnal Dipping Calculation

# Dip calc for subject 70435
dip_calc(hypnos_proc, subj = '70435')

# Dip calc for subject 70439
dip_calc(hypnos_proc, subj = '70439')

# Identify the outlier value
hypnos_proc %>%
  dplyr::filter(ID =='70439') %>%
  dplyr::arrange(desc(DBP)) %>%
  head(3) %>%
  dplyr::select(SBP, DBP, BP_CLASS, DATE_TIME, MAP, HR, RPP, WAKE, VISIT)

# Re-calculated HYPNOS data after screening out the outlier value
hypnos_proc_recalc <- process_data(bp_hypnos, bp_type = "abpm",
                                                  sbp = "SYST",
                                                  dbp = "DIAST",
                                            date_time = "DATE.TIME",
                                                   hr = "HR",
                                                   pp = "PP",
                                                  map = "MAP",
                                                  rpp = "RPP",
                                                   id = "ID",
                                                visit = "VISIT",
                                                 wake = "WAKE",
                                                  DUL = 130)

# Run dip calc again but with filtered HYPNOS data
dip_calc(hypnos_proc_recalc, subj = c('70439') )

# Figure 5 - dipping plots

# Dipping category plots before and after removing outlier
p1 = dip_class_plot(hypnos_proc, subj = c('70435','70439'))
p2 = dip_class_plot(hypnos_proc_recalc, subj = c('70435','70439'))

p1 = p1 + ggforce::geom_circle(aes(x0 = -4.4, y0 = 9.6, r = 3), inherit.aes = FALSE, col = "orange", size = 1.5)
p2 = p2 + ggforce::geom_circle(aes(x0 = -5.5, y0 = 2.7, r = 3), inherit.aes = FALSE, col = "orange", size = 1.5)

pdf(file = paste(path,"figure5_dip_calc.pdf", sep = ""), width = 12, height = 6)
p1 + p2
dev.off()

## Section 6.3 - Nocturnal Metrics
# bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))

# SBP-related Nocturnal metrics
# bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))[[4]][,c(1:10)]

# DBP-related Nocturnal metrics
# bp_sleep_metrics(hypnos_proc_recalc, subj = c('70435','70439'))[[4]][,c(1:3, 11:17)]


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


# Run session info
sessionInfo()

