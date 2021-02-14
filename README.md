
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
for analyzing blood pressure data using a variety of statistical metrics
and visualizations to bring more clarity to CVD.

The  package includes two sample data sets:

  - `hypnos_data`: a sample of a larger [HYPNOS
    study](https://clinicaltrials.gov/ct2/show/NCT02454153) containing
    ABPM data for multiple subjects using continuous monitoring devices
  - `bp_jhs`: a single-subject data set from a [2019 pilot
    study](https://dataverse.harvard.edu/dataverse/r4r) containing
    non-ABPM data from a self-monitoring Omron Evolv device

### Installation

<!-- You can install the released version of bp from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("bp") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

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
statistical literature and provide visualizations. Perhaps the most
useful user-friendly feature of the package is the ability to generate a
visualization report to discern relationships and assess blood pressure
stage progression among subjects.

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
resulting processed dataframe is used for all other functions.

Unique to the `bp` package is the ability to create additional column
that might not originally be present in the supplied data set. At
current, the following additional columns will be created:

  - `TIME_OF_DAY` - Corresponds to the Time of Day (Morning, Afternoon,
    Evening, or Night) based on `DATE_TIME` column
  - `DAY_OF_WEEK` - Corresponds to the Day of the week: a useful column
    for table visuals. Based on `DATE_TIME` column
  - `SBP_CATEGORY` - Systolic Blood Pressure Stages (Low, Normal,
    Elevated, Stage 1, Stage 2, Crisis) as defined by the American Heart
    Association
  - `DBP_CATEGORY` - Diastolic Blood Pressure Stages (Low, Normal,
    Elevated, Stage 1, Stage 2, Crisis) as defined by the American Heart
    Association

See examples below for further details.

### Available Metrics

The package will then utilize the above variables to calculate various
metrics from medical and statistical literature in order to quantify and
classify the variability of the readings into their respective
categories of hypertension (normal, elevated, or hypertensive).

The following metrics are currently offered through the `bp` package:

| Function   | Metric Name                                | Source                                                                        |
| ---------- | ------------------------------------------ | ----------------------------------------------------------------------------- |
| arv        | Average Real Variability                   | [Mena et al (2005)](https://doi.org/10.1097/01.hjh.0000160205.81652.5a)       |
| bp\_center | Mean and Median                            | [Amaro Lijarcio et al (2006)](https://doi.org/10.1016/j.ejim.2006.07.023)     |
| bp\_mag    | Blood Pressure Magnitude (peak and trough) | [Munter et al (2011)](https://doi.org/10.1097/HJH.0b013e32834cf213)           |
| bp\_range  | Blood Pressure Range                       | [Levitan et al (2013)](https://doi.org/10.1038/jhh.2013.19)                   |
| cv         | Coefficient of Variation                   | [Munter et al (2011)](https://doi.org/10.1097/HJH.0b013e32834cf213)           |
| sv         | Successive Variation                       | [Munter et al (2011)](https://doi.org/10.1097/HJH.0b013e32834cf213)           |
| dip\_calc  | Nocturnal Dipping % and Classification     | [Okhubo et al (1997)](https://academic.oup.com/ajh/article/10/11/1201/148906) |

## Example - HYPNOS data

There are two main steps involved with the `bp` package: The data
processing step and the functionality / analysis step.

1.  Load and process data into a new usable dataframe for all further
    analysis using the `process_data` function

<!-- end list -->

``` r
#devtools::install_github("johnschwenck/bp")
library(bp)

## Load hypnos_data
data(hypnos_data)

## Process hypnos_data
hypnos_proc <- process_data(hypnos_data, 
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
```

**NOTE:** the `process_data` function is insensitive to capitalization
of the supplied data column names. For this example, even though the
original column name “SYST” exists in the `hypnos_data`, “syst” is still
an acceptable name to be given to the function as shown. For emphasis,
all of the above column names were intentionally entered using the wrong
capitalization.

`SBP` and `DBP` must be specified for any other functions to work
properly.

2.  Using the newly processed `hypnos_proc`, we can now calculate
    various metrics. Now that the `hypnos_data` has been processed into
    `hypnos_proc`, we can now instead rely on this new dataframe to
    calculate various metrics and visualizations. The calculation of the
    nocturnal dipping classification is shown below, using a subset of
    only two of the subjects for comparison (subjects 70417 and 70435):

<!-- end list -->

``` r
dip_calc(hypnos_proc, subj = c(70417, 70435))
#> [[1]]
#> # A tibble: 8 x 6
#> # Groups:   ID, VISIT [4]
#>      ID VISIT  WAKE avg_SBP avg_DBP     N
#>   <int> <int> <int>   <dbl>   <dbl> <int>
#> 1 70417     1     0    116.    56       4
#> 2 70417     1     1    130     66.5    11
#> 3 70417     2     0    142     63.2     4
#> 4 70417     2     1    135.    63.9     9
#> 5 70435     1     0    100     62       3
#> 6 70435     1     1    130.    82.2    12
#> 7 70435     2     0    110     65.3     3
#> 8 70435     2     1    133.    80.3    11
#> 
#> [[2]]
#> # A tibble: 4 x 6
#> # Groups:   ID [2]
#>      ID VISIT dip_sys class_sys dip_dias class_dias
#>   <int> <int>   <dbl> <chr>        <dbl> <chr>     
#> 1 70417     1 -0.110  dipper     -0.158  dipper    
#> 2 70417     2  0.0510 reverse    -0.0100 non-dipper
#> 3 70435     1 -0.233  extreme    -0.245  extreme   
#> 4 70435     2 -0.173  dipper     -0.186  dipper
```

In terms of statistical metrics, the `bp_stats` function aggregates many
of the variability and center metrics into one table which makes
comparing the different measures to one another very convenient. Let’s
suppose for this example that we wanted to further analyze these two
subjects by their `SBP_CATEGORY` and were not concerned about DBP
output: we would set `bp_type = 1` to subset on only SBP measures, and
we would include `add_groups = "SBP_category"` as an additional argument
(note that capitalization does not matter).

``` r
bp_stats(hypnos_proc, subj = c(70417, 70435), add_groups = "sbp_category", bp_type = 1)
#> # A tibble: 21 x 16
#> # Groups:   ID, VISIT, WAKE [8]
#>       ID     N VISIT  WAKE SBP_CATEGORY SBP_mean SBP_med    SD   ARV    SV
#>    <int> <int> <int> <int> <fct>           <dbl>   <dbl> <dbl> <dbl> <dbl>
#>  1 70417     4     1     0 Normal           116.    116.  2.06  1.33  1.83
#>  2 70417     1     1     1 Normal           118     118  NA    NA    NA   
#>  3 70417     5     1     1 Elevated         124     124   2.24  2.5   3.24
#>  4 70417     3     1     1 Stage 1          135.    136   3.21  3     3.61
#>  5 70417     2     1     1 Stage 2          144     144   1.41  2     2   
#>  6 70417     2     2     0 Stage 1          133     133   1.41  2     2   
#>  7 70417     2     2     0 Stage 2          151     151   0     0     0   
#>  8 70417     1     2     1 Normal           120     120  NA    NA    NA   
#>  9 70417     1     2     1 Elevated         121     121  NA    NA    NA   
#> 10 70417     5     2     1 Stage 1          134.    132   4.28  2.75  4.15
#> # ... with 11 more rows, and 6 more variables: CV <dbl>, SBP_max <dbl>,
#> #   SBP_min <dbl>, SBP_range <dbl>, Peak <dbl>, Trough <dbl>
```

The `bp` package has multiple visualization tools available:

  - `bp_hist` - Histograms for various stages of blood pressure
  - `bp_scatter` - Scatter plot of the blood pressure stages as denoted
    by the American Heart Association
  - `dow_tod_plots` - Table visuals to break down readings by time of
    day and day of week
  - `bp_report` - An exportable blood pressure report that aggregates
    the individual visualization outputs in a clean digestible format

Here is an example of the individual visual function `bp_scatter` for
subject \#70417:

``` r
bp_scatter(hypnos_proc, subj = 70417)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
