# 

- Single Site, Anomaly, Longitudinal\*

- Single Site, Anomaly, Longitudinal\*

## Usage

``` r
evp_ss_anom_la(process_output, output_level, filter_variable, facet = NULL)
```

## Arguments

- process_output:

  the output provided by the `evp_process` function

- output_level:

  the level of output to be displayed: `patient` or `row`

- filter_variable:

  the single variable the output should display

- facet:

  columns the user would like to facet by

## Value

if analysis was executed by year or greater, a P Prime control chart is
returned with outliers marked with orange dots

        if analysis was executed by month or smaller, an STL regression is
        conducted and outliers are marked with red dots. the graphs representing
        the data removed in the regression are also returned
