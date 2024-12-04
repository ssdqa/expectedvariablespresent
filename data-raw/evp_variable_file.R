## code to prepare `evp_variable_file` dataset goes here

evp_variable_file_omop <- tibble::tibble(variable = c('Sample OMOP Variable'),
                                         domain_tbl = c('condition_occurrence'),
                                         concept_field = c('condition_concept_id'),
                                         date_field = c('condition_start_date'),
                                         codeset_name = c('dx_hypertension'),
                                         filter_logic = c(NA))

usethis::use_data(evp_variable_file_omop, overwrite = TRUE)


evp_variable_file_pcornet <- tibble::tibble(variable = c('Sample PCORnet Variable'),
                                            domain_tbl = c('diagnosis'),
                                            concept_field = c('dx'),
                                            date_field = c('admit_date'),
                                            vocabulary_field = c('dx_type'),
                                            codeset_name = c('sample_codeset_dx'),
                                            filter_logic = c(NA))

usethis::use_data(evp_variable_file_pcornet, overwrite = TRUE)
