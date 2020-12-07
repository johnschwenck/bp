
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bp

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/johnschwenck/bp.svg?branch=master)](https://travis-ci.com/johnschwenck/bp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/johnschwenck/bp?branch=master&svg=true)](https://ci.appveyor.com/project/johnschwenck/bp)
<!-- badges: end -->

## bp: Blood Pressure Analysis for R

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

### Installation

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

### Available Metrics

The package will then utilize the above variables to calculate various
metrics from medical and statistical literature in order to quantify and
classify the variability of the readings into their respective
categories of hypertension (normal, elevated, or hypertensive).

  - `arv` - Average Real Variability
  - `bp_mag` - Blood Pressure Magnitude (peak and trough)
  - `bp_range` - Blood Pressure Range
  - `cv` - Coefficient of Variation
  - `sv` - Successive Variation
  - `dip_calc` - Nocturnal Dipping % and Classification

## Example - HYPNOS data

There are two main steps involved:

1.  Load and process data into a new usable dataframe for all further
    analysis using the `process_data` function

<!-- end list -->

``` r
#devtools::install_github("johnschwenck/bp")
library(bp)

## Load hypnos_data
data(hypnos_data)

## Process hypnos_data
data <- process_data(hypnos_data, 
                     sbp = 'syst', 
                     dbp = 'diast', 
                     bp_datetime = 'date.time', 
                     hr = 'hr', 
                     pp = 'PP', 
                     map = 'MaP', 
                     rpp = 'Rpp', 
                     id = 'id', 
                     visit = 'Visit', 
                     wake = 'wake')
#> Warning in process_data(hypnos_data, sbp = "syst", dbp = "diast", bp_datetime =
#> "date.time", : DATE column found in data and coerced to as.Date() format.
```

**NOTE:** the `process_data` function is insensitive to capitalization
of the supplied data column names. For this example, even though the
original column name “SYST” exists in the `hypnos_data`, “syst” is still
an acceptable name to be given to the function as shown. For emphasis,
all of the above column names were intentionally entered using the wrong
capitalization.

`SBP` and `DBP` must be specified for any other functions to work
properly.

2.  Using the newly processed `data`, calculate various metrics. Now
    that the `hypnos_data` has been processed, all functions will now
    rely on `data` instead. The calculation of the nocturnal dipping
    classification is shown below:

<!-- end list -->

``` r
dip_calc(data)
#> [[1]]
#> # A tibble: 20 x 6
#> # Groups:   ID, VISIT [10]
#>       ID VISIT  WAKE avg_SBP avg_DBP     N
#>    <int> <int> <int>   <dbl>   <dbl> <int>
#>  1 70417     1     0    117.    55.7     7
#>  2 70417     1     1    129.    67.3    23
#>  3 70417     2     0    136.    60.5     8
#>  4 70417     2     1    136.    65.6    17
#>  5 70422     1     0    139.    58.2     5
#>  6 70422     1     1    151.    65.9    17
#>  7 70422     2     0    125.    60.6     7
#>  8 70422     2     1    152.    69.5    14
#>  9 70424     1     0    110.    51.8     6
#> 10 70424     1     1    128.    67.8    20
#> 11 70424     2     0    113     52.3     6
#> 12 70424     2     1    123.    61.5    17
#> 13 70435     1     0    106.    63       6
#> 14 70435     1     1    129.    82.1    23
#> 15 70435     2     0    106.    63.5     6
#> 16 70435     2     1    133.    77.5    23
#> 17 70439     1     0    167     62.6     8
#> 18 70439     1     1    160.    69.3    14
#> 19 70439     2     0    149.    60.8     6
#> 20 70439     2     1    144.    56.8    17
#> 
#> [[2]]
#> # A tibble: 10 x 4
#> # Groups:   ID [5]
#>       ID VISIT      dip classification
#>    <int> <int>    <dbl> <chr>         
#>  1 70417     1 -0.0926  non-dipper    
#>  2 70417     2  0.00450 reverse       
#>  3 70422     1 -0.0819  non-dipper    
#>  4 70422     2 -0.174   dipper        
#>  5 70424     1 -0.141   dipper        
#>  6 70424     2 -0.0848  non-dipper    
#>  7 70435     1 -0.179   dipper        
#>  8 70435     2 -0.197   dipper        
#>  9 70439     1  0.0442  reverse       
#> 10 70439     2  0.0329  reverse
```
