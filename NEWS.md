# bp 1.0.0

* First release.
* Added a `NEWS.md` file to track changes to the package.

# bp 1.0.1

* Fixed missing path argument warning for bp_report
* Added GitHub URL to DESCRIPTION file with BugReports link
* Added install.packages("bp") instructions in ReadMe
* Fixed minor dependency issues in the NAMESPACE


# bp 1.0.2

* Fixed minor bug in dow_tod_plots if DATE is missing
* Added warnings to bp_sleep_metrics if input is not as expected
* Treat night as sleep in ABPM data in the absence of sleep/wake information

# bp 2.0.0

### Major Updates:
* Complete overhaul to process_data() function by adding the following changes:
    - Added a data_process_helpers.R file to improve the readability and efficiency of background processes of process_data() function
    - Added argument to aggregate data (agg) for successive measurements and collapse if desired
    - Added argument to differentiate between HBPM, ABPM, and AP data types
    - Added argument to differentiate the end-of-day (eod)
    - Restructured the way that BP stages are calculated in order to obtain a 2-to-1 mapping of SBP & DBP to a respective category
    - Added the ability to change the input date/time format and specify time zone
    - Added the ability to adjust the order of the data (chronological vs reverse-chronological)
* Addition of new visualization functions:
    - bp_ts_plots: Time series plots
    - dip_class_plot: Plot for dipping percentage classification
* Addition of new Hypertension metrics:
    - dip_calc: Nocturnal dipping percentage
    - bp_sleep_metrics: Sleep metrics that include weighted SD (wSD), morning blood pressure surge (PW_mbps and ST_mbps), and morningness-eveningness (ME_avg and ME_diff)
* Added 4 new data sets:
    - bp_preg
    - bp_children
    - bp_ghana
    - bp_rats
    
### Bug Fixes:
* Set UTC time zone for bp_jhs for consistency of time conversion across systems
* More flexible output of bp_report for either direct plotting or saving grob object for later
* Renamed hypnos_data.rda to bp_hypnos.rda for consistency

    
# bp 2.0.1
* Minor bug fixes related to user-supplied data with missing values
    - Adjustments to process_data function (data_screen issue, various back-end helper functions, etc)
    - Omits NA values in various metric calculations and nocturnal metrics
    - Fixed various plotting issues
* Corrected medical references surrounding sleep vs nocturnal language


# bp 2.1.0
* Restructuring of bp_type argument in various functions to consist of 'both', 'sbp', 'dbp' instead of 0, 1, 2
    - NOTE: This may affect legacy functionality / backward compatibility
* Removed deprecated functions from documentation: arv, cv, sv (replaced with bp_arv, bp_cv, bp_sv)
* Fixed histogram binning for missing / Null values

