# EVP Sample Variable File – PCORnet

A sample version of the file structure expected for the
evp_variable_file parameter in the `evp_process` function. The user
should recreate this file structure and include their own variables.

## Usage

``` r
evp_variable_file_pcornet
```

## Format

### `evp_variable_file_pcornet`

A data frame with 6 columns

- variable:

  A string label for the variable of interest

- domain_tbl:

  The name of the CDM table where the variable can be found

- concept_field:

  The field in the default_tbl that should be used to join to the
  codeset

- date_field:

  The date field in the default_tbl that should be used to filter the
  dataset to the cohort period and for longitudinal analyses

- vocabulary_field:

  field in the `default_tbl` that defines the vocabulary type of the
  concept (i.e. dx_type)

- codeset_name:

  The name of the codeset as found in the specs directory; file
  extension should not be included

- filter_logic:

  optional; a string to be parsed as logic to filter the default_tbl and
  better identify the variable of interest

## Details

Please note that the codesets should be stored in the
`file_subdirectory` established when `ssdqa.gen::initialize_dq_session`
is executed.

Examples of appropriately structured codeset files are attached to the
ssdqa.gen package and can be accessed with `ssdqa.gen::`. Please note
that if the vocabulary_field is not NULL (i.e. for diagnosis or
procedures concepts), the vocabulary_id field in the codeset must be
included.
