
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bp

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/johnschwenck/bp.svg?branch=master)](https://travis-ci.com/johnschwenck/bp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/johnschwenck/bp?branch=master&svg=true)](https://ci.appveyor.com/project/johnschwenck/bp)
<!-- badges: end -->

### bp: Blood Pressure Analysis for R

Cardiovascular disease (CVD) is the leading cause of death worldwide
with Hypertension, specifically, affecting over 1.1 billion people
annually. The goal of the  package is to provide a comprehensive toolbox
for analyzing blood pressure data using a variety of statistical methods
and metrics in an effort to bring more clarity to CVD.

The  package includes two sample data sets:

  - `hypnos_data`: a sample of a larger HYPNOS study containing ABPM
    data for multiple subjects using continuous monitoring devices
  - `bp_jhs`: a single-subject data set from a [2019 pilot
    study](https://dataverse.harvard.edu/dataverse/r4r) containing
    non-ABPM data from a self-monitoring Omron Evolv device

## Installation

You can install the released version of bp from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bp")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johnschwenck/bp")
```

For installation with vignettes:

``` r
devtools::install_github("johnschwenck/bp", build_vignettes = TRUE)
```

### Intended Functionality

The `bp` package is designed to allow the user to initialize a processed
dataframe by specifying any combination of the following variables
present in the user-supplied data set (with the minimum requirement that
`SBP` and `DBP` are included). The package will then utilize the
processed dataframe to calculate various metrics from medical and
statistical literature.

The package has the ability to make use of the following physiological
variables (expressed as integers):

  - Systolic Blood Pressure (`SBP`) measured in mmHg
  - Diastolic Blood Pressure (`DBP`) measured in mmHg
  - Heart Rate (`HR`) measured in bpm
  - Pulse Pressure (`PP`) measured in mmHg which is calculated as SBP -
    DBP
  - Mean Arterial Pressure (`MAP`) measured in mmHg
  - Rate Pressure Product (`RPP`) which is calculated as SBP multiplied
    by resting HR

The data can be further refined on a more granular scale, depending on
the type of data supplied. In most instances, ABPM data will include
some kind of binary column corresponding to awake vs asleep which the
user will assign when initializing the processed data. Further, most
blood pressure data sets contain a timestamp associated with each
reading. The additional variables are as follows:

  - `DATE_TIME` combination such as `12/1/2020 13:42:07` (`as.POSIXct`
    format)
  - `ID` of individuals, if more than one
  - `VISIT` corresponding to the visit of each individual, if more than
    one (integer)
  - `WAKE` as a binary indicator where 1 denotes awake and 0 denotes
    asleep (binary 1 or 0)

After all available variables are identified and processed, the
resulting processed dataframe is used for all other functions. See
examples below for further details.

The package will then utilize the above variables to calculate various
metrics from medical and statistical literature (ARV, morning surge %,
etc) in order to quantify the variability of the readings, classify
subjects as either “dipper” or “non-dipper”, and cluster subjects (if
more than one) into their respective categories of hypertension (normal,
elevated, or hypertensive).

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bp)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
