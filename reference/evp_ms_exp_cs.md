# 

- Multi Site, Exploratory, Cross-Sectional\*

- Multi Site, Exploratory, Cross-Sectional\*

## Usage

``` r
evp_ms_exp_cs(
  process_output,
  output_level,
  large_n = FALSE,
  large_n_sites = NULL,
  facet = NULL
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

- facet:

  columns the user would like to facet by

## Value

a heat map displaying the proportion of patients/rows that meet criteria
for each of the variables found in process_output at each of site
