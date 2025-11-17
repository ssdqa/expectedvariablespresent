# Compute variable distribution (PCORnet)

Compute variable distribution (PCORnet)

## Usage

``` r
compute_evp_pcnt(
  cohort,
  grouped_list,
  time = FALSE,
  evp_variable_file = expectedvariablespresent::evp_variable_file_pcornet
)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, `person_id`,
  `start_date`, and `end_date`

- grouped_list:

  list of columns that should be used to group the table

- time:

  logical to determine whether the function is being run as part of
  `compute_fot` or not

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

## Value

dataframe with patient/row counts and proportions that are computed per
group defined in grouped_list, and if time = TRUE, for each time period
defined in compute_fot
