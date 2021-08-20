
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
#' Type 2 diabetes, not using insulin therapy and with a glycosylated hemoglobin (HbA_1c) value at least 6.5
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
"bp_hypnos"


#' Pregnancy Day Assessment Clinic Data
#'
#' The \code{bp_preg} data set includes 209 women each with 30 minute recordings during the Pregnancy Day
#' Assessment Clinic (PDAC) observation window for up to a maximum of 240 minutes (i.e. a maximum
#' of 8 total readings per subject per observation window in addition to an initial "booking" reading
#' before the PDAC assessment). This recent study in obstetrics and gynecology investigated
#' pregnancy-induced hypertension (PIH) and pre-eclampsia (PE) prediction by analyzing cardiac and
#' physiological information to determine whether the blood pressure assessment of the first observation
#' window of 1 hour (60 minutes) was sufficient relative to the standard 4 hour (240 minute) window.
#'
#' (McCarthy EA, Carins TA, Hannigan Y, Bardien N, Shub A, Walker S. 2015)
#'
#' Licensed under a CC-0 1.0 (Creative Commons) Universal Public Domain Dedication License
#'
#' @source
#' \doi{10.5061/dryad.0bq15}
#'
#' @format A data frame containing 55 variables related to physiological assessment during pregnancy.
#' The data variable dictionary is as follows:
#' \describe{
#'
#' \item{Abdominal.pain}{Binary indicator for whether an individual exhibits abdominal pain (1 = yes) or not (0 = no)}
#' \item{Accelerations}{Fetal heart rate accelerations}
#' \item{Admission}{Admission from PDAC}
#' \item{Age}{Maternal age at estimated date of confinement}
#' \item{ALP}{Alkaline phosphatase u/L}
#' \item{ALT}{Alanaine transaminase u/L}
#' \item{AN_PET}{Binary indicator for Antenatal diagnosis of pre-eclampsia 1 = yes, 0 = no}
#' \item{Antihypertensive.Meds}{Current antihypertensive medication}
#' \item{Asymptomatic}{Binary indicator for whether an individual is asymptomatic (1 = yes) or not (0 = no)}
#' \item{Baseline}{Baseline fetal heart rate (normal, bradycardia, tachycardia)}
#' \item{BMI}{Body Mass Index}
#' \item{Creatinine}{micromol/L}
#' \item{CTG}{Cardiotocogram}
#' \item{D_att_Dryad}{Date of attendance modified by adding a random number of days between -7 and +7}
#' \item{DBP}{Diastolic Blood Pressure}
#' \item{Decelerations}{Fetal heart rate decelerations}
#' \item{DOB_Dryad}{Maternal date of birth modified by adding a random number of days between -7 and +7}
#' \item{DrRV}{Medical review during PDAC}
#' \item{EDC_Dryad}{Estimated date of confinement modified by adding a random number of days between -7 and +7}
#' \item{EmCS}{Emergency Caesarean section}
#' \item{Final.Diagnosis.}{Final diagnosis: 0 = nil, 1 = pregnancy induced hypertension, 2 = pre-eclampsia}
#' \item{GA_att}{Gestational age (weeks) at attendance}
#' \item{GGT}{Gamma glutamyl transaminase u/L}
#' \item{Grav}{Gravidity: number of pregnancies including the current one}
#' \item{Headache}{Binary indicator for whether an individual has a headache (1 = yes) or not (0 = no)}
#' \item{Ht}{Maternal Height (cm)}
#' \item{Hyperreflexia}{Binary indicator for whether an individual exhibits Hyperreflexia (1 = yes) or not (0 = no)}
#' \item{IOL}{Induction of labour}
#' \item{ID}{Subject ID #}
#' \item{IP_PET}{Intrapartum diagnosis of pre-eclampsia 1 = yes, 0 = no}
#' \item{Nausea}{Binary indicator for whether an individual exhibits Nausea (1 = yes) or not (0 = no)}
#' \item{Oedema}{Binary indicator for whether an individual exhibits Oedema (1 = yes) or not (0 = no)}
#' \item{Para}{Parity: number of pregnancies proceding to 20 or more weeks, not including the current one}
#' \item{PHx_ASA}{Prescribed aspirin in this pregnancy}
#' \item{PHx_Eclampsia}{Past history of eclampsia}
#' \item{PHx_EssBP}{Past history of essential hypertension}
#' \item{PHx_PET}{Past history of pre-eclampsia}
#' \item{PHx_Smoker}{Smoking history}
#' \item{PIERS}{Pre-eclampsia Integrated Estimate of Risk (\%)}
#' \item{PN_PET}{Postnatal diagnosis of pre-eclampsia 1 = yes, 0 = no}
#' \item{Platelets}{10^9 per mL}
#' \item{PrevPDAC}{Number of previous PDAC assessments this pregnancy}
#' \item{Privacy}{Generic privacy consent form permits participation in audit}
#' \item{SBP}{Systolic Blood Pressure}
#' \item{SOB}{Binary indicator for whether an individual exhibits Shortness of Breath (SOB) (1 = yes) or not (0 = no)}
#' \item{Safe.for.discharge.at.1.hour.}{Binary indicator for whether an individual is safe for discharge at 1 hour
#' (1 = yes) or not (0 = no)}
#' \item{Safe.for.discharge.at.1.hour....Other..please.specify.}{Binary indicator for whether an individual is safe
#' for discharge at 1 hour - other - comments}
#' \item{Self.discharge}{Self-discharge from PDAC}
#' \item{Time_Elapsed}{30 minute recordings during the Pregnancy Day Assessment Clinic (PDAC) observation
#' window for up to a maximum of 240 minutes including the "Booking" recording}
#' \item{Urate}{mmol/L}
#' \item{Urea}{micromol/L}
#' \item{UrinePCR}{Spot urine protein:creatinine ratio mg/mmol}
#' \item{Variability}{Fetal heart rate variability as judged visually}
#' \item{Visual.Disturbances}{Binary indicator for whether an individual exhibits Visual Disturbances (1 = yes)
#' or not (0 = no)}
#' \item{Wt}{Pre-pregnancy or early pregnancy weight (kg)}
#'
#' }
#'
"bp_preg"


#' B-Proact1v Children Data
#'
#' The \code{bp_children} data set contains information on 1,283 children from Bristol, UK,
#' who each took three blood pressure readings per visit over the course of two observation
#' periods (at ages 9 and 11) and had their body mass index (BMI), physical activity, and
#' sedentary time information recorded. The study examined how sedentary behavior and physical
#' activity affected children progressing through primary school to better understand the
#' relationship between elevated blood pressure in children and its impact on the development
#' of cardiovascular disease into adulthood
#'
#' (Solomon-Moore E, Salway R, Emm-Collison L, Thompson JL, Sebire SJ, Lawlor DA, Jago R (PI). 2020).
#'
#' Licensed under a CC-BY Creative Commons Attribution 4.0 International
#'
#' @source
#' Original Paper: \doi{10.1371/journal.pone.0232333}
#'
#' Data: \doi{10.5281/zenodo.1049587}
#'
#' Principal Investigator (PI): Russ Jago (University of Bristol)
#'
#' @format A data frame containing 15 variables pertaining to blood pressure and physical
#' activity as follows:
#' \describe{
#'
#' \item{id}{Child ID}
#' \item{reading}{Index of the blood pressure reading}
#' \item{sbp}{Systolic Blood Pressure (mmHg)}
#' \item{dbp}{Diastolic Blood Pressure (mmHg)}
#' \item{gender}{1 = Male\cr 2 = Female}
#' \item{hh_educ}{Household Highest Education:\cr\cr
#' 1 = Up to GCSE/O level or equiv\cr
#' 2 = A level/NVQ or equiv\cr
#' 3 = Degree/HND or equiv\cr
#' 4 = Higher degree (MSc/PhD) or equiv\cr}
#' \item{visit}{Visit #}
#' \item{age}{Age in Years}
#' \item{ht}{Height (ft)}
#' \item{wt}{Weight (lbs)}
#' \item{bmi}{Body Mass Index}
#' \item{N.valid.days.all}{Number of days of child accelerometer data overall}
#' \item{avg.mins.all}{Average minutes per day over all valid days}
#' \item{sed.avg.mins.all}{Average sedentary minutes per day over all valid days}
#' \item{mvpa.avg.mins.all}{Average minutes of Moderate to Vigorous Physical Activity (MVPA) per day over all valid days}
#'
#' }
#'
"bp_children"


#' Task Shifting and Blood Pressure Control in Ghana Data
#'
#' The \code{bp_ghana} data set includes 757 subjects across 32 community health centers who
#' were partitioned into intervention groups according to a pragmatic cluster-randomized trial
#' with 389 in the health insurance coverage (HIC) group and 368 in another group consisting
#' of a combination of HIC with a nurse-led task-shifting strategy for hypertension control
#' (TASSH) (this group is denoted TASSH + HIC). This study was an effort to assess the
#' comparative effectiveness of HIC alone versus the combination of TASSH + HIC on reducing
#' systolic blood pressure among patients with uncontrolled hypertension in Ghana. Baseline
#' blood pressure measurements and 12 month follow-up results were collected among subjects,
#' 85\% of whom had 12 month data present
#'
#' (Ogedegbe G, Plange-Rhule J, Gyamfi J, Chaplin W, Ntim M, Apusiga K, Iwelunmor J, Awudzi KY,
#' Quakyi KN, Mogavero JN, Khurshid K, Tayo B, Cooper R. 2019).
#'
#' Licensed under a CC-0 1.0 (Creative Commons) Universal Public Domain Dedication License
#'
#' @source
#' Original Paper: \doi{10.1371/journal.pmed.1002561}
#'
#' Data: \doi{10.5061/dryad.16c9m51}
#'
#' @format A data frame containing 21 variables pertaining to blood pressure as follows:
#' \describe{
#'
#' \item{ID}{Subject ID }
#' \item{Time_Elapsed}{How much time elapsed between readings: Baseline measurement, 6 month follow-up,
#' 12 month follow-up}
#' \item{SBP}{Systolic Blood Pressure}
#' \item{DBP}{Diastolic Blood Pressure}
#' \item{Age}{Age in Years}
#' \item{Gender}{0 = Male \cr 1 = Female}
#' \item{EducationLevel}{Highest Degree Earned:\cr\cr
#' 1 = No Schooling\cr
#' 2 = Primary Schooling (Grades 1 to 6)\cr
#' 3 = Junior Secondary Schooling (JSS) (Grades 7-8)\cr
#' 4 = Secondary School (Grades 9-11)\cr
#' 5 = Completed Secondary School\cr
#' 6 = Technical school certificate\cr
#' 7 = Some college but no degree\cr
#' 8 = Associate degree\cr
#' 9 = Graduate or Professional school (MD, JD, etc.)\cr
#' 999 = Not specified\cr}
#' \item{EmploymentStatus}{0 = Unemployed \cr 1 = Employed}
#' \item{Literacy}{0 = Illiterate \cr 1 = Literate}
#' \item{Smoking}{1 = Smoker \cr 2 = Occasional \cr 3 = Ex-Smoker \cr 4 = Non-Smoker}
#' \item{Income}{Income level expressed in home currency - Ghanaian cedi (GhC)}
#' \item{SiteNumber}{Site Location in Ghana:\cr\cr
#' 1 = Suntreso\cr
#' 2 = Ananekrom\cr
#' 3 = Manhiya\cr
#' 4 = Apatrapa\cr
#' 5 = Nkawie\cr
#' 6 = Afrancho\cr
#' 7 = Kumsai South\cr
#' 8 = Mampongteng\cr
#' 9 = Mampong\cr
#' 10 = Bomfa\cr
#' 11 = Ejura\cr
#' 12 = Kofiase\cr
#' 13 = Kokofu\cr
#' 14 = Asuofia\cr
#' 15 = Konongo\cr
#' 16 = Subirisu\cr
#' 17 = Effiduase\cr
#' 18 = Abuakwa\cr
#' 19 = Tafo\cr
#' 20 = KMA\cr
#' 21 = Methodist\cr
#' 22 = Kenyasi\cr
#' 23 = Asonamaso\cr
#' 24 = Juansa\cr
#' 25 = Juaso\cr
#' 26 = Sekyredumase\cr
#' 27 = Nkenkaasu\cr
#' 28 = Berekese\cr
#' 29 = Bekwai\cr
#' 30 = Dwease\cr
#' 31 = Kuntanese \cr
#' 32 = Foase \cr
#' }
#' \item{CVRisk}{Cardiovascular Risk Assessment (\%) at Baseline: \cr\cr
#' 1) <10\% \cr
#' 2) 10-20\% \cr
#' 3) 20-30\% \cr
#' 4) 30 to 40\% \cr
#' 999= Missing Data}
#' \item{BMI}{Body Mass Index}
#' \item{BMIClassification}{Classification of BMI:\cr\cr
#' 1 = Underweight (<18.5 kg/m^2)\cr
#' 2 = Normal (18.5 to 24.9 kg/m^2)\cr
#' 3 = Overweight (25.0 to 29.9 kg/m^2)\cr
#' 4 = Obese (>30 kg/m^2)\cr
#' }
#' \item{PhysicalActivity}{All activity in weighted MET minutes (per week) at Baseline}
#' \item{TrtorCtrl}{0 = Control\cr 1 = Treatment}
#' \item{RuralUrban}{0 = Rural\cr 1 = Urban}
#' \item{DoctorsAvailable}{# of doctors on staff}
#' \item{NursesAvailable}{# of nurses on staff}
#' \item{Patientsannually}{# of patients seen annually}
#'
#' }
#'
"bp_ghana"


#' Blood Pressure in Salt-Sensitive Dahl Rats Data
#'
#' The \code{bp_rats} data set includes arterial blood pressure waveform time series data
#' of the SS(n = 9) and SS.13 (n = 6) genetic strains of Dahl rats sampled at 100 Hz who
#' were each administered a low and high salt diet. It is taken from the study of Bugenhagen
#' et al.(2010) which sought to investigate the origins of the baroreflex dysfunction in
#' salt-sensitive Dahl rats on Hypertension.
#'
#' (Goldberger A., Amaral L., Glass L., Hausdorff J., Ivanov P. C., Mark R., Bugenhagen S.M.,
#' Cowley A.W. Jr, Beard D.A., ... \& Stanley H. E. 2000).
#'
#' Licensed under a ODC-BY (Creative Commons) Open Data Commons Attribution License 1.0
#'
#' @source
#' Original Paper: \doi{10.1152/physiolgenomics.00027.2010}
#'
#' Data: \doi{10.13026/C20597}
#'
#' @format A data frame containing 5 variables pertaining to arterial continuous waveform blood
#' pressure as follows:
#' \describe{
#'
#' \item{rat_type}{The type of a particular Dahl Rat (either SS or SSBN13) corresponding to the sodium intake administered:\cr\cr
#' ss_hs = SS rat given the high sodium diet\cr
#' ss_ls = SS rat given the low sodium diet\cr
#' ssbn13_hs = SSBN13 rat given the high sodium diet\cr
#' ssbn13_ls = SSBN13 rat given the low sodium diet\cr}
#' \item{rat_ID}{ID # corresponding to a particular rat. There are 9 SS rats and 6 SSBN13 rats.}
#' \item{ABP}{Continuous waveform data of each rat's arterial (blood) pressure sampled at 100 Hz.}
#' \item{time_sec}{Amount of time elapsed, expressed in seconds}
#' \item{time_min}{Amount of time elapsed, expressed in minutes}
#'
#' }
#'
"bp_rats"
