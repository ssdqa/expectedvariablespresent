# 

- Multi Site, Anomaly, Cross-Sectional\*

- Multi Site, Anomaly, Cross-Sectional\*

## Usage

``` r
evp_ms_anom_cs(
  process_output,
  output_level,
  large_n = FALSE,
  large_n_sites = NULL,
  text_wrapping_char = 60
)
```

## Arguments

- process_output:

  the output provided by the `evp_process` function

- output_level:

  the level of output to be displayed: `patient` or `row`

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

- text_wrapping_char:

  integer value indicating the point at which axis label text should
  begin to wrap

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of
rows/patients for a given variable, and the size of the dot represents
the mean proportion across all sites

        if there were no groups eligible for analysis, a heat map showing the proportion
        and a dot plot showing each site's average standard deviation away from the mean
        proportion is returned instead
