# **Multi-Site, Anomaly, Longitudinal**

**Multi-Site, Anomaly, Longitudinal**

## Usage

``` r
evp_ms_anom_la(
  process_output,
  output_level,
  filter_variable,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output from `evp_process`

- output_level:

  the level of output that should be shown (`person` or `row`)

- filter_variable:

  the variable that should be used to generate output

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

three graphs:

1.  line graph that shows the smoothed proportion of a variable across
    time computation with the Euclidean distance associated with each
    line

2.  line graph that shows the raw proportion of a variable across time
    computation with the Euclidean distance associated with each line

3.  a bar graph with the Euclidean distance value for each site, with
    the average proportion as the fill

THIS GRAPH SHOWS ONLY ONE VARIABLE AT A TIME!
