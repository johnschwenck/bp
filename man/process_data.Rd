% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_process.R
\name{process_data}
\alias{process_data}
\title{Data Pre-Processor}
\usage{
process_data(
  data,
  bp_type = c("hbpm", "abpm", "ap"),
  ap = NULL,
  time_elap = NULL,
  sbp = NULL,
  dbp = NULL,
  date_time = NULL,
  id = NULL,
  group = NULL,
  wake = NULL,
  visit = NULL,
  hr = NULL,
  pp = NULL,
  map = NULL,
  rpp = NULL,
  DoW = NULL,
  ToD_int = NULL,
  eod = NULL,
  data_screen = TRUE,
  inc_low = TRUE,
  inc_crisis = TRUE,
  dt_fmt = "ymd HMS"
)
}
\arguments{
\item{data}{User-supplied dataset containing blood pressure data. Must
contain data for Systolic blood pressure and Diastolic blood pressure at a
minimum.}

\item{bp_type}{Required argument specifying which of the three BP data types
("HBPM", "ABPM", or "AP") the input data is. Default \code{bp_type} set to "HBPM".
This argument determines which processing steps are necessary to yield sensible
output.}

\item{ap}{(For AP data only) Required column name (character string) corresponding
to continuous Arterial Pressure (AP) (mmHg). Note that this is a required argument
so long as bp_type = "AP". Ensure that bp_type is set accordingly.}

\item{time_elap}{(For AP data only) Column name corresponding to the time elapsed
for the given AP waveform data.}

\item{sbp}{Required column name (character string) corresponding to Systolic Blood
Pressure (mmHg)}

\item{dbp}{Required column name (character string) corresponding to Diastolic Blood
Pressure (mmHg)}

\item{date_time}{Optional column name (character string) corresponding to Date/Time,
but HIGHLY recommended to supply if available.}

\item{id}{Optional column name (character string) corresponding to subject ID. Typically
needed for data corresponding to more than one subject. For one-subject datasets, ID
will default to 1 (if ID column not found in dataset)}

\item{group}{Optional column name (character string) corresponding to an additional
grouping variable that can be used to further break down data. NOTE that this simply
sets the column as "GROUP" so that other functions recognize which column to use as
the grouping variable.}

\item{wake}{Optional column name (character string) corresponding to sleep status. A
WAKE value of 1 indicates that the subject is awake and 0 implies asleep.}

\item{visit}{Optional column name (character string) corresponding to Visit number}

\item{hr}{Optional column name (character string) corresponding to Heart Rate (bpm)}

\item{pp}{Optional column name (character string) corresponding to Pulse Pressure
(SBP - DBP). If not supplied, it will be calculated automatically.}

\item{map}{Optional column name (character string) corresponding to Mean Arterial
Pressure}

\item{rpp}{Optional column name (character string) corresponding to Rate Pulse
Pressure (SBP * HR). If not supplied, but HR column available, then
RPP will be calculated automatically.}

\item{DoW}{Optional column name (character string) corresponding to the Day of the Week.
If not supplied, but DATE or DATE_TIME columns available, then DoW will be created
automatically. DoW values must be abbreviated as such \code{c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")}}

\item{ToD_int}{Optional vector that overrides the default interval for the Time-of-Day periods.
By default, the Morning, Afternoon, Evening, and Night periods are set at 6, 12, 18, 0 respectively,
where 0 corresponds to the 24th hour of the day (i.e. Midnight). By inputting a vector for the
\code{ToD_int} function argument, the default period can be re-arranged accordingly.
For example, ToD_int = c(5, 13, 18, 23) would correspond to a period for
Morning starting at 5:00 (until 13:00), Afternoon starting at 13:00 (until 18:00),
Evening starting at 18:00 (until 23:00), and Night starting at 23:00 (until 5:00)}

\item{eod}{Optional argument to adjust the delineation for the end of day (eod). For individuals who
do not go to bed early or work night-shifts, for example, this argument adjusts the end of day so
that any readings in the early morning (i.e. past midnight but before they wake up) are not grouped with
the next day's readings.}

\item{data_screen}{Optional logical argument; default set to TRUE. Screens for extreme values in the data
for both \code{SBP} and \code{DBP} according to Omboni, et al (1995) paper - Calculation of Trough:Peak
Ratio of Antihypertensive Treatment from Ambulatory Blood Pressure: Methodological Aspects}

\item{inc_low}{Optional logical argument dictating whether or not to include the "Low" category for BP
classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.}

\item{inc_crisis}{Optional logical argument dictating whether or not to include the "Crisis" category for BP
classification column (and the supplementary SBP/DBP Category columns). Default set to TRUE.}

\item{dt_fmt}{Optional argument that specifies the input date/time format (dt_fmt). Default set to "ymd HMS"
but can take on any format specified by the lubridate package.

See https://lubridate.tidyverse.org/reference/parse_date_time.html for more details.}
}
\value{
A processed dataframe object with standardized column names and formats to use with the rest of bp package functions. The following standardized column names are used throughout
\item{BP_TYPE}{One of AP, HBPM or ABPM}
\item{ID}{Subject ID}
\item{SBP}{Systolic Blood Pressure}
\item{DBP}{Diastolic Blood Pressure}
\item{SBP_CATEGORY}{Ordinal, SBP characterization into "Normal"<"Elevated"<"Stage 1"< "Stage 2"}
\item{DBP_CATEGORY}{Ordinal, DBP characterization into "Normal"<"Elevated"<"Stage 1"< "Stage 2"}
\item{BP_CLASS}{Blood pressure categorization based on paired values (SBP, DBP) into one of the stages}
\item{HR}{Heart Rate}
\item{MAP}{Mean Arterial Pressure}
\item{PP}{Pulse Pressure, SBP-DBP}
\item{DATE_TIME}{Date and time in POSIXct format}
\item{DATE}{Date only in Date format}
\item{MONTH}{Month, integer from 1 to 12}
\item{DAY}{Day, integer from 1 to 31}
\item{YEAR}{Four digit year}
\item{DAY_OF_WEEK}{Ordinal, with "Sun"<"Mon"<"Tue"<"Wed"<"Thu"<"Fri"<"Sat"}
\item{TIME}{Time in character format}
\item{HOUR}{Integer, from 0 to 23}
\item{TIME_OF_DAY}{One of "Morning", "Afternoon", "Evening" or "Night"}
}
\description{
A helper function to assist in pre-processing the user-supplied
input data in a standardized format for use with other functions in the \code{bp} package.
See Vignette for further details.
}
\examples{
# Load bp_hypnos
data("bp_hypnos")

# Process data for bp_hypnos
hypnos_proc <- process_data(bp_hypnos,
                              bp_type = 'abpm',
                              sbp = 'syst',
                              dbp = 'diast',
                              date_time = 'date.time',
                              hr = 'hr',
                              pp = 'PP',
                              map = 'MaP',
                              rpp = 'Rpp',
                              id = 'id',
                              visit = 'Visit',
                              wake = 'wake',
                              data_screen = FALSE)

hypnos_proc


# Load bp_jhs data
data("bp_jhs")

# Process data for bp_jhs
# Note that bp_type defaults to "hbpm" and is therefore not specified
jhs_proc <- process_data(bp_jhs,
                         sbp = "Sys.mmHg.",
                         dbp = "Dias.mmHg.",
                         date_time = "DateTime",
                         hr = "Pulse.bpm.")

jhs_proc

}
