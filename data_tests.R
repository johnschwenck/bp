library(ggplot2)
library(lubridate)
library(dplyr)

source('R/data_process.R')

# Test df
data <- readxl::read_excel('C:\\Users\\John\\Documents\\CGM Research\\Data\\ABPM\\70417-1 ABPM.xlsx') # Delete
data <- data[, -8]
data <- data[, c(5,3,8,6,7)]
data <- as.matrix(data)

# Personal
data2 <- read.csv('C:\\Users\\John\\Documents\\Final Data (Bike Trip)\\Blood Pressure\\Blood_Pressure.csv')
data2 <- process_data(data2,
                      bp_datetime = "DateTime",
                      sbp = "Sys.mmHg.",
                      dbp = "Dias.mmHg.",
                      map = "map",
                      hr = "Pulse.bpm.",
                      pp = "pp")


# HYPNOS - ABPM
data <- read.csv('C:\\Users\\John\\Documents\\CGM Research\\Data\\ABPM\\abpm_combined.csv')
data <- process_data(data,
                     bp_datetime = "date.time",
                     sbp = "syst",
                     dbp = "diast",
                     map = "map",
                     hr = "hr",
                     pp = "pp",
                     rpp = "rpp",
                     id = "id",
                     wake = "wake",
                     visit = "visit")



# HYPNOS PLOTS
data$DATE <- as.Date(data$DATE)
dt_obj <- lubridate::ymd_hms(data$DATE_TIME)
data$dt_obj <- format(dt_obj, "%H:%M:%S")
names(data)[15] <- "time_fmt"

# One Subject
subject = unique(data$ID)
visit = 1

pdf(file = "C:\\Users\\John\\Documents\\CGM Research\\Data\\cgm_abpm_plots.pdf")
for(i in 1:length(subject)){

  data_sub <- data[which(data$ID == subject[i] & data$VISIT == visit),]



  # Add in CGM data
  cgm <- read.csv('C:\\Users\\John\\Documents\\CGM Research\\Data\\cgm_abpm.csv')
  cgm$time <- as.POSIXct(cgm$time, format = "%m/%d/%Y %H:%M")
  cgm$time_fmt <- format(as.POSIXct(cgm$time, format = "%m/%d/%Y %H:%M"), "%H:%M:%S")
  names(cgm)[2] <- "DATE_TIME"
  names(cgm)[1] <- "ID"

  cgm <- cgm[which(cgm$ID == subject[i]),]



  tmp1 = cgm[which( (cgm$DATE_TIME >= min(data_sub$DATE_TIME)) & (cgm$DATE_TIME <= max(data_sub$DATE_TIME))),]
  #plot(tmp1$DATE_TIME, tmp1$gl, type = "l", ylim = c(min(data$HR), max(tmp1$gl)), main = "CGM vs SBP & HR")
  #points(data$DATE_TIME, data$SBP, col= 'red')
  #points(data$DATE_TIME, data$HR, col= 'blue')


  plot <- ggplot() +
    geom_line(data = tmp1, aes(x = DATE_TIME, y = gl, color = device)) +
    geom_point(data = data_sub, aes(x = DATE_TIME, y = SBP, shape = 3, color = "Syst BP")) +
    geom_point(data = data_sub, aes(x = DATE_TIME, y = HR, shape = 1, color = "HR")) + scale_shape_identity() +
    scale_colour_manual(values = c("abbott" = "lightsalmon1", "dexcom" = 'mediumpurple1', 'HR' = 'red', 'Syst BP' = 'green4')) +
    labs(title = 'CGM vs SBP & HR', subtitle = paste("Subject #", subject[i], " | ", "Visit #", data_sub$VISIT[1])) +
    geom_vline(xintercept = min(data_sub[which(data_sub$WAKE == 0),]$DATE_TIME), color = 'gray57', linetype = 'dashed') +
    geom_vline(xintercept = max(data_sub[which(data_sub$WAKE == 0),]$DATE_TIME), color = 'gray57', linetype = 'dashed') +
    theme_minimal()

  print(plot)
}

dev.off()























# All HR and all SBP
ggplot(data = data, mapping = aes(x = time_fmt)) +
  geom_point(aes(y = SBP), color = "darkred") +
  geom_point(aes(y = HR), color = "steelblue") +#, alpha = 1/5)
  ggtitle("Systolic BP vs HR over a 24 hour period")

# HR and SBP by VISIT
ggplot(data = data, mapping = aes(x = time_fmt)) +
  geom_point(aes(y = SBP, color= factor(VISIT))) +
  geom_point(aes(y = HR, color = factor(VISIT))) +#, alpha = 1/5)
  ggtitle("Systolic BP vs HR by Visit # over a 24 hour period")




### Combined Plots

#
# ggplot(data = cgm, mapping = aes(x = time_fmt)) +
#   geom_point(aes(y = gl, color = factor(Activity)))



#cgm_abpm_time <- full_join(data, cgm, by = "time_fmt")
cgm_abpm_datetime <- full_join(data, cgm, by = c("DATE_TIME", "time_fmt"))

cgm_abpm_datetime$roll3 <- rollmean(cgm_abpm_datetime$SBP, 3, na.pad = T)

ggplot(cgm_abpm_datetime, mapping = aes(x = time_fmt, y = gl, color = factor(Activity))) +
  geom_point(alpha = 1/5) +
  geom_point(aes(y = SBP, color = "SBP")) +
  geom_point(aes(y = HR, color = "HR")) +
  ggtitle("CGM, Systolic BP, and HR throughout 24 hours", subtitle = "Subject: 70417")



tmp = cgm_abpm_datetime %>% group_by(DATE_TIME = cut(DATE_TIME, breaks = "5 min")) %>% summarise(gl = mean(gl))
tmp$DATE_TIME <- format(tmp$DATE_TIME, "%H:%M")
View(tmp)




tmp_cgm = aggregate(gl ~ time_fmt, data = cgm_abpm_datetime, mean, na.rm = T)
tmp_bp = aggregate(SBP ~ time_fmt, data = cgm_abpm_datetime, mean, na.rm = T)
tmp_hr = aggregate(HR ~ time_fmt, data = cgm_abpm_datetime, mean, na.rm = T)

tmp = left_join(tmp_cgm, tmp_bp, by = "time_fmt")
tmp = left_join(tmp, tmp_hr, by = "time_fmt")

plot.ts(tmp$gl, ylim = c(50,250), main = "Subject: 70417 GL, SBP, and HR Average over 24 hours")
points(tmp$SBP, col = 'red')
points(tmp$HR, col = 'blue')














StatLm <- ggproto("StatLm", Stat,
                  required_aes = c("x", "y"),

                  compute_group = function(data, scales, params, n = 100, formula = y ~ x) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = seq(rng[1], rng[2], length = n))

                    mod <- lm(formula, data = data)
                    grid$y <- predict(mod, newdata = grid)

                    grid
                  }
)

stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, n = 50, formula = y ~ x,
                    ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, formula = formula, na.rm = na.rm, ...)
  )
}

ggplot(cgm_abpm_datetime, aes(time_fmt, gl)) +
  geom_point() +
  stat_lm(formula = gl ~ time_fmt) +
  stat_lm(formula = gl ~ time_fmt, geom = "point", colour = "red", n = 20)
