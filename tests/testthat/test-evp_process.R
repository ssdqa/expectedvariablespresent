
## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(evp_process(cohort = cht,
                          multi_or_single_site = 'test',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'omop'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(evp_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'test',
                          omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(evp_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'test'))
})


## Generally checking that code runs
test_that('evp ss/ms exp nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'evp_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  evp_vars <- tidyr::tibble('variable' = 'hypertension',
                            'domain_tbl' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date',
                            'codeset_name' = 'dx_hypertension',
                            'filter_logic' = NA)

  expect_no_error(evp_process(cohort = cohort,
                              evp_variable_file = evp_vars,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              omop_or_pcornet = 'omop'))
})

test_that('evp ss anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'evp_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  evp_vars <- tidyr::tibble('variable' = 'hypertension',
                            'domain_tbl' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date',
                            'codeset_name' = 'dx_hypertension',
                            'filter_logic' = NA)

  expect_no_error(evp_process(cohort = cohort,
                              evp_variable_file = evp_vars,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))
})

test_that('evp ms anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'evp_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  evp_vars <- tidyr::tibble('variable' = 'hypertension',
                            'domain_tbl' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date',
                            'codeset_name' = 'dx_hypertension',
                            'filter_logic' = NA)

  expect_no_error(evp_process(cohort = cohort,
                              evp_variable_file = evp_vars,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))
})

test_that('evp ss/ms exp at -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'evp_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  evp_vars <- tidyr::tibble('variable' = 'hypertension',
                            'domain_tbl' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date',
                            'codeset_name' = 'dx_hypertension',
                            'filter_logic' = NA)

  expect_no_error(evp_process(cohort = cohort,
                              evp_variable_file = evp_vars,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              time = TRUE,
                              time_period = 'year',
                              time_span = c('2000-01-01', '2005-01-01'),
                              omop_or_pcornet = 'omop'))
})

test_that('testing pcornet version', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  sess <- argos$new()

  set_argos_default(sess)

  config('results_name_tag', '')

  expect_error(evp_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'pcornet'))
})
