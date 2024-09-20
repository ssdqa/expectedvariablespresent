## code to prepare `evp_variable_file` dataset goes here

evp_variable_file <- tibble(variable = c('Sample OMOP Variable', 'Sample PCORnet Variable'),
                            default_tbl = c('condition_occurrence', 'lab_result_cm'),
                            field_name = c('condition_concept_id', 'lab_loinc'),
                            date_field = c('condition_start_date', 'result_date'),
                            codeset_name = c('sample_codeset_dx', 'sample_codeset_lab'),
                            filter_logic = c(NA, 'result_num >= 8'))

usethis::use_data(evp_variable_file, overwrite = TRUE)
