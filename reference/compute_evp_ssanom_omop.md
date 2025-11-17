# Single site anomaly no time processing for EVP

Single site anomaly no time processing for EVP

## Usage

``` r
compute_evp_ssanom_omop(
  cohort,
  grouped_list,
  evp_variable_file = expectedvariablespresent::evp_variable_file_omop
)
```

## Arguments

- cohort:

  table of cohort members with at least `site`, `person_id`,
  `start_date`, and `end_date`

- grouped_list:

  list of columns that should be used to group the table

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

  - `codeset_name` the name of the codeset file; DO NOT include the file
    extension

  - `filter_logic` a string indicating filter logic that should be
    applied to achieve the desired variable; optional

## Value

one dataframe with the jaccard similarity index for each concept group
provided in the concept file
