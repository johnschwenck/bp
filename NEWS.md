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

# bp 1.0.3

* Set UTC time zone for bp_jhs for consistency of time conversion across systems
* More flexible output of bp_report for either direct plotting or saving grob object for later
