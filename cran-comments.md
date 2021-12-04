## Submission Comments 0.1
* Due to a 'utf8' package issue (https://github.com/r-hub/rhub/issues/374), devtools::check_rhub(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")) was run for the rhub check and passed
* Local R CMD Check warning:  "'qpdf' is needed for checks on size reduction of PDFs" is specific to the local machine. Not an error. Passes Travis and Appveyor.
* One time error: "Error in curl::curl_fetch_memory(url, handle = h) : Failed FTP upload: 550" from devtools::check_win_devel() (Issue: https://github.com/r-lib/devtools/issues/1435). Used httr::set_config(use_proxy(url="10.3.100.207",port=8080)) to fix.
* check_rhub() issue with DOI / URL reference in certain function documentation even though links work as intended (dip_calc, dip_class_plot)

## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

