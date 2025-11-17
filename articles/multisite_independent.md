# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

## Multi-Site Exploratory Analysis

The process for the exploratory analysis is the same for both the
cross-sectional and longitudinal configurations.

First, execute either of the **Single Site, Exploratory** analyses,
configured appropriately for your study, against each data source.

``` r
library(expectedvariablespresent)

my_table <- evp_process(cohort = my_cohort,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        time = T / F,
                        ...)
```

Then, combine these results into a single table with the different sites
delineated in the `site` column.

``` r
my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n)
```

## Multi-Site Anomaly Detection Analysis

For anomaly detection analysis, start by executing the same steps as the
exploratory analysis. Then, you will execute the relevant anomaly
detection algorithm against the resulting table. See below for the
different processes for cross-sectional and longitudinal analysis.

### Cross-Sectional

For a cross-sectional analysis, the `compute_dist_anomalies` and
`detect_outliers` functions, both available through the `squba.gen`
package, should be executed against your results. Copy the code below,
inputting the table you generated.

If you would like to apply anomaly detection against patient-level
counts, `analysis_level` should be set to `prop_pt_variable`. If you
would prefer to use row counts, set it to `prop_row_variable`. The
`p_value` can be selected by the user.

``` r
# First execute the compute_dist_anomalies function
df_start <- compute_dist_anomalies(df_tbl = my_table,
                                   grp_vars = c('variable'),
                                   var_col = analysis_level,
                                   denom_cols = c('variable', 'total_pt_ct', 'total_row_ct'))

# Then, use that output as input for the detect_outliers function
df_final <- detect_outliers(df_tbl = df_start,
                            tail_input = 'both',
                            p_input = p_value,
                            column_analysis = analysis_level,
                            column_variable = 'variable')
```

### Longitudinal

For a longitudinal analysis, the `ms_anom_euclidean` function, available
through the `squba.gen` package, should be executed against your
results. Copy the code below, inputting the data you generated.

If you would like to apply anomaly detection against patient-level
counts, `analysis_level` should be set to `prop_pt_variable`. If you
would prefer to use row counts, set it to `prop_row_variable`.

``` r
df <- ms_anom_euclidean(fot_input_tbl = my_table,
                        grp_vars = c('site', 'variable'),
                        var_col = analysis_level)
```
