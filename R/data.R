#' Blood Pressure - 1 Subject - John Schwenck
#'
#' Single-subject self-monitored blood pressure readings over 108 days (April 15, 2019 - August 01, 2019).
#' This data set has been processed and uploaded to the Harvard Dataverse for public use. It contains
#' variables pertaining to Date/Time, Systolic BP, Diastolic BP, and Heart Rate. The data assumes a threshold
#' blood pressure of 135 / 85 which is used to calculate excess amounts.
#'
#' @source \doi{10.7910/DVN/EA1SAP}
#'
#' @format A data frame with the following columns:
#' \describe{
#'
#' \item{DateTime}{A POSIXct-formatted column corresponding to the date and time of the corresponding reading in local time.}
#' \item{Month}{Integer. The month corresponding to the Date column.}
#' \item{Day}{Integer. The day of the month corresponding to the Date column.}
#' \item{Year}{Integer. The year corresponding to the Date column.}
#' \item{DayofWk}{Character. The day of the week corresponding to the Date column.}
#' \item{Time}{Character. The time corresponding to the DateTime column.}
#' \item{Hour}{Integer. The hour corresponding to the DateTime column.}
#' \item{Meal_Time}{Character. The estimated meal time corresponding to the DateTime column.}
#' \item{Sys.mmHg.}{Integer. The systolic blood pressure reading.}
#' \item{Dias.mmHg.}{Integer. The diastolic blood pressure reading.}
#' \item{bpDelta}{Integer. The difference between the Sys.mmHg. and Dias.mmHg. column. This is also known as the Pulse Pressure.}
#' \item{Pulse.bpm.}{Integer. The heart rate value (measured in beats per minute -- bpm).}
#'
#' }
#'
"bp_jhs"


#' HYPNOS Study - 5 Subject Sample
#'
#' ABPM measurements for 5 subjects with Type II diabetes. These data are part of a larger study sample that
#' consisted of patients with Type 2 diabetes recruited from the general community. To be eligible, patients with
#' Type 2 diabetes, not using insulin therapy and with a glycosylated hemoglobin (HbA$_1c$) value at least 6.5
#'
#' @format A data frame with the following columns:
#' \describe{
#'
#' \item{NR.}{Integer. The index corresponding to the reading of a particular subject for a given visit.}
#' \item{DATE.TIME}{Character. The date-time value corresponding to the given reading.}
#' \item{SYST}{Integer. The systolic blood pressure reading.}
#' \item{MAP}{Integer. The mean arterial pressure value.}
#' \item{DIAST}{Integer. The diastolic blood pressure reading.}
#' \item{HR}{Integer. The heart rate value (measured in beats per minute -- bpm).}
#' \item{PP}{Integer. The pulse pressure value calculated as the systolic value - the diastolic value.}
#' \item{RPP}{Integer. The rate pressure product calculated as the systolic reading multiplied by the heart rate value.}
#' \item{WAKE}{Integer. A logical indicator value corresponding to whether or not a subject is awake (\code{WAKE = 1}) or not.}
#' \item{ID}{Integer. A unique identifier for each subject.}
#' \item{VISIT}{Integer. A value associated with the visit number or a particular subject.}
#' \item{DATE}{Character. A date-valued column indicating the date of the given reading. Dates are specified according to their
#' actual date (i.e. 01:00 corresponds to the next date, even if the subject is awake. Awake-state is indicated via \code{WAKE}).}
#'
#' }
#'
#'
"hypnos_data"
