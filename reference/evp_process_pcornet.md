# Expected Variables Present – PCORnet

Expected Variables Present – PCORnet

## Usage

``` r
evp_process_pcornet(
  cohort,
  evp_variable_file = expectedvariablespresent::evp_variable_file_pcornet,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  output_level = "row",
  age_groups = NULL,
  p_value = 0.9,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  A dataframe with the cohort of patients for your study. Should include
  the columns:

  - `person_id`

  - `start_date`

  - `end_date`

  - `site`

- evp_variable_file:

  CSV file with information about each of the variables that should be
  examined in the function. contains the following columns:

  - `variable` a label for the variable captured by the associated
    codeset

  - `default_tbl` CDM table where data related to the codeset is found

  - `concept_field` concept_id field with codes from the associated
    codeset

  - `date_field` a date field in the `default_tbl` that should be used
    for over time analyses

  - `vocabulary_field` PCORNET ONLY; field in the `default_tbl` that
    defines the vocabulary type of the concept (i.e. dx_type) if this
    field is used, the codeset should have a `vocabulary_id` column that
    defines the appropriate vocabularies for each concept

  - `codeset_name` the name of the codeset file; DO NOT include the file
    extension

  - `filter_logic` a string indicating filter logic that should be
    applied to achieve the desired variable; optional

- multi_or_single_site:

  Option to run the function on a single vs multiple sites

  - `single`: run the function for a single site

  - `multi`: run the function for multiple sites

- anomaly_or_exploratory:

  Option to conduct an exploratory or anomaly detection analysis.
  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- output_level:

  the level of output to use for an AUC computation, exclusive to
  `ms_anom_at`; either `patient` or `row` – defaults to `row`

- age_groups:

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and include it as the
  `age_groups` function parameter:

  - `min_age`: the minimum age for the group (i.e. 10)

  - `max_age`: the maximum age for the group (i.e. 20)

  - `group`: a string label for the group (i.e. 10-20, Young Adult,
    etc.)

  If you would *not* like to stratify by age group, leave the argument
  as NULL

- p_value:

  the p value to be used as a threshold in the multi-site anomaly
  detection analysis

- time:

  a logical that tells the function whether you would like to look at
  the output over time

- time_span:

  when time = TRUE, this argument defines the start and end dates for
  the time period of interest. should be formatted as c(start date, end
  date) in yyyy-mm-dd date format

- time_period:

  when time = TRUE, this argument defines the distance between dates
  within the specified time period. defaults to `year`, but other time
  periods such as `month` or `week` are also acceptable

## Value

a dataframe with patient/row counts & proportions for each concept set
listed in `evp_concept_file`. this output should then be used in the
`evp_output` function to generate an appropriate visualization
