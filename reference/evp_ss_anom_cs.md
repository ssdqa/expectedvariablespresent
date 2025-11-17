# 

- Single Site, Anomaly, Cross-Sectional\*

- Single Site, Anomaly, Cross-Sectional\*

## Usage

``` r
evp_ss_anom_cs(process_output, facet = NULL)
```

## Arguments

- process_output:

  the output provided by the `evp_process` function

- facet:

  columns the user would like to facet by

## Value

a heat map displaying the Jaccard similarity index between each of the
variables any variables without a relationship and any self-to-self
relationships are dropped
