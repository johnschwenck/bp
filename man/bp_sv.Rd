% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bp_sv.R
\name{bp_sv}
\alias{bp_sv}
\title{Successive Variation (SV)}
\usage{
bp_sv(
  data,
  inc_date = FALSE,
  subj = NULL,
  bp_type = c("both", "sbp", "dbp"),
  add_groups = NULL,
  inc_wake = TRUE
)
}
\arguments{
\item{data}{Required argument. Pre-processed dataframe with SBP and DBP columns
with optional ID, VISIT, WAKE, and DATE columns if available.
Use \code{process_data} to properly format data.}

\item{inc_date}{Optional argument. Default is FALSE. As ABPM data typically
overlaps due to falling asleep on one date and waking up on another, the \code{inc_date}
argument is typically kept as FALSE, but the function will work regardless. Setting
\code{inc_date = TRUE} will include these dates as a grouping level.}

\item{subj}{Optional argument. Allows the user to specify and subset specific subjects
from the \code{ID} column of the supplied data set. The \code{subj} argument can be a single
value or a vector of elements. The input type should be character, but the function will
comply with integers so long as they are all present in the \code{ID} column of the data.}

\item{bp_type}{Optional argument. Determines whether to calculate SV for SBP
values or DBP values, or both. For \strong{both SBP and DBP} ARV values use bp_type = 'both',
for \strong{SBP-only} use bp_type = 'sbp, and for \strong{DBP-only} use bp_type = 'dbp'.
If no type specified, default will be set to 'both'}

\item{add_groups}{Optional argument. Allows the user to aggregate the data by an
additional "group" to further refine the output. The supplied input must be a
character vector with the strings corresponding to existing column names of the
processed \code{data} input supplied. Capitalization of \code{add_groups} does not matter.
Ex: \code{add_groups = c("Time_of_Day")}}

\item{inc_wake}{Optional argument corresponding to whether or not to include \code{WAKE}
in the grouping of the final output (if \code{WAKE} column is available). By default,
\code{inc_wake = TRUE} which will include the \code{WAKE} column in the groups by which
to calculate the respective metrics.}
}
\value{
A tibble object with a row corresponding to each subject, or alternatively
a row corresponding to each date if inc_date = TRUE. The resulting tibble consists of:
\itemize{

   \item \code{ID}: The unique identifier of the subject. For single-subject datasets, ID = 1
   \item \code{VISIT}: (If applicable) Corresponds to the visit # of the subject, if more than 1
   \item \code{WAKE}: (If applicable) Corresponds to the awake status of the subject (0 = asleep |
   1 = awake)
   \item \code{SV_SBP} / \code{SV_DBP}: Calculates the square root of the average squared differences
   between successive measurements. The resulting value averages across the granularity
   grouping for however many observations are present.
   \item N: The number of observations for that particular grouping. If \code{inc_date = TRUE},
   \code{N} corresponds to the number of observations for that date. If \code{inc_date = FALSE}, N
   corresponds to the number of observations for the most granular grouping available (i.e.
   a combination of \code{ID}, \code{VISIT}, and \code{WAKE})
   \item Any add_groups variables supplied to function argument will be present as a column in the
   resulting tibble.

}
}
\description{
Calculate the successive variation (SV) at various levels of granularity
based on what is supplied (ID, VISIT, WAKE, and / or DATE)for either SBP,
DBP, or both. SV is a measure of dispersion that takes into account the
temporal structure of the data and relies on the sum of squared differences
in successive observations, unlike the average real variability (ARV)
which relies on the sum of absolute differences.
$$SV = sqrt(sum(x_{i+1} - x_i)^2/n-1)$$
}
\details{
\strong{NOTE:} The canonical standard deviation, independent of the temporal
structure using the sample average, is added for comparison:
$$SD = sqrt(sum(x_{i+1} - xbar)^2/n-1)$$
}
\examples{
# Load data
data(bp_hypnos)
data(bp_jhs)

# Process bp_hypnos
hypnos_proc <- process_data(bp_hypnos, sbp = "SYST", dbp = "DIAST", date_time = "date.time",
id = "id", wake = "wake", visit = "visit", hr = "hr", pp ="pp", map = "map", rpp = "rpp")
# Process bp_jhs data
jhs_proc <- process_data(bp_jhs, sbp = "Sys.mmHg.", dbp = "Dias.mmHg.", date_time = "DateTime",
hr = "Pulse.bpm.")

# SV Calculation
bp_sv(hypnos_proc)
bp_sv(jhs_proc, add_groups = c("meal_time"))
# Notice that meal_time is not a column from process_data, but it still works
}
