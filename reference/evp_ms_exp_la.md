# 

- Multi Site, Exploratory, Longitudinal\*

- Multi Site, Exploratory, Longitudinal\*

## Usage

``` r
evp_ms_exp_la(
  process_output,
  output_level,
  filter_variable,
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

- filter_variable:

  the single variable the output should display

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

- facet:

  columns the user would like to facet by

## Value

a line graph displaying the proportion of patients/rows for each
variable & site over the user-specified time period.
