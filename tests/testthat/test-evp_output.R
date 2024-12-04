
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(evp_output(process_output = tbl_test,
                          output_function = 'evp_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test <- tidyr::tibble('site' = 'a',
                            'total_pt_ct' = 100,
                            'total_row_ct' = 1000,
                            'variable_pt_ct' = 50,
                            'variable_row_ct' = 500,
                            'prop_pt_variable' = 0.5,
                            'prop_row_variable' = 0.5,
                            'variable' = 'test')

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ss_exp_cs'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ss_exp_cs'))

})


test_that('single site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble('concept1' = 'drug',
                            'concept2' = 'condition',
                            'cocount' = 50,
                            'concept1_ct' = 75,
                            'concept2_ct' = 100,
                            'concept_count_union' = 65,
                            'jaccard_index' = 0.8,
                            'concept1_prop' = 0.6,
                            'concept2_prop' = 0.5,
                            'grp' = 'site')

  expect_no_error(evp_output(process_output = tbl_test,
                             output_function = 'evp_ss_anom_cs'))

})


test_that('multi site, exploratory, no time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'total_pt_ct' = c(10, 20, 30),
                            'total_row_ct' = c(100, 200, 300),
                            'variable_pt_ct' = c(5, 15, 25),
                            'variable_row_ct' = c(50, 150, 250),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8),
                            'prop_row_variable' = c(0.5, 0.7, 0.8),
                            'variable' = c('test', 'test', 'test'))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ms_exp_cs'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ms_exp_cs'))

})


test_that('multi site, anomaly detection, no time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'total_pt_ct' = c(10, 20, 30),
                            'total_row_ct' = c(100, 200, 300),
                            'variable_pt_ct' = c(5, 15, 25),
                            'variable_row_ct' = c(50, 150, 250),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8),
                            'prop_row_variable' = c(0.5, 0.7, 0.8),
                            'variable' = c('test', 'test', 'test'),
                            'mean_val' = c(0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3),
                            'analysis_eligible' = c('yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier'))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ms_anom_cs'))

  expect_no_error(evp_output(process_output = tbl_test %>% dplyr::mutate(anomaly_yn = 'no outlier in group'),
                             output_level = 'patient',
                             output_function = 'evp_ms_anom_cs'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ms_anom_cs'))

})


test_that('single site, exploratory, across time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01'),
                            'time_increment' = c('year', 'year', 'year'),
                            'total_pt_ct' = c(10, 20, 30),
                            'total_row_ct' = c(100, 200, 300),
                            'variable_pt_ct' = c(5, 15, 25),
                            'variable_row_ct' = c(50, 150, 250),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8),
                            'prop_row_variable' = c(0.5, 0.7, 0.8),
                            'variable' = c('test', 'test', 'test'))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ss_exp_la'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ss_exp_la'))

})

test_that('multi site, exploratory, across time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'b', 'b', 'b'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01',
                                             '2018-01-01', '2019-01-01', '2020-01-01'),
                            'time_increment' = c('year', 'year', 'year',
                                                 'year', 'year', 'year'),
                            'total_pt_ct' = c(10, 20, 30, 10, 20, 30),
                            'total_row_ct' = c(100, 200, 300, 100, 200, 300),
                            'variable_pt_ct' = c(5, 15, 25, 5, 15, 25),
                            'variable_row_ct' = c(50, 150, 250, 50, 150, 250),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8, 0.5, 0.7, 0.8),
                            'prop_row_variable' = c(0.5, 0.7, 0.8, 0.5, 0.7, 0.8),
                            'variable' = c('test', 'test', 'test', 'test', 'test', 'test'))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ms_exp_la',
                             filter_variable = 'test'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ms_exp_la',
                          filter_variable = 'test'))

})


test_that('single site, anomaly detection, across time - year', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01'),
                            'time_increment' = c('year', 'year', 'year'),
                            'total_pt_ct' = c(10, 20, 30),
                            'total_row_ct' = c(100, 200, 300),
                            'variable_pt_ct' = c(5, 15, 25),
                            'variable_row_ct' = c(50, 150, 250),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8),
                            'prop_row_variable' = c(0.5, 0.7, 0.8),
                            'variable' = c('test', 'test', 'test'))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ss_anom_la',
                             filter_variable = 'test'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ss_anom_la',
                          filter_variable = 'test'))

})


test_that('single site, anomaly detection, across time - month', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a'),
                            'time_start' = c('2018-01-01', '2018-02-01', '2018-03-01',
                                             '2018-04-01', '2018-05-01'),
                            'time_increment' = c('month', 'month', 'month', 'month', 'month'),
                            'total_pt_ct' = c(10, 20, 30, 40, 50),
                            'total_row_ct' = c(100, 200, 300, 400, 500),
                            'variable_pt_ct' = c(5, 15, 25, 35, 45),
                            'variable_row_ct' = c(50, 150, 250, 350, 450),
                            'prop_pt_variable' = c(0.5, 0.7, 0.8, 0.9, 0.91),
                            'prop_row_variable' = c(0.5, 0.7, 0.8, 0.9, 0.91),
                            'variable' = c('test', 'test', 'test', 'test', 'test'),
                            'observed' = c(0.5, 0.6, 0.7, 0.8, 0.9),
                            'season' = c(1,2,3,4,5),
                            'trend' = c(1,2,3,4,5),
                            'remainder' = c(0.46, 0.57, 0.69, 0.82, 0.88),
                            'seasonadj' = c(1,2,3,4,5),
                            'anomaly' = c('Yes', 'No', 'No', 'No', 'Yes'),
                            'anomaly_direction' = c(-1,0,0,0,1),
                            'anomaly_score' = c(1,2,3,4,5),
                            'recomposed_l1' = c(0.44, 0.6, 0.5, 0.49, 0.46),
                            'recomposed_l2' = c(0.84, 0.8, 0.8, 0.89, 0.86),
                            'observed_clean' = c(0.46, 0.57, 0.69, 0.82, 0.88))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ss_anom_la',
                             filter_variable = 'test'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ss_anom_la',
                          filter_variable = 'test'))

})


test_that('multi site, anomaly detection, across time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01',
                                             '2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'variable' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses',
                                         'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'prop_pt_variable' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94, 0.94),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94, 0.94),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9,0.9,0.9,0.9,0.9))

  expect_no_error(evp_output(process_output = tbl_test,
                             output_level = 'patient',
                             output_function = 'evp_ms_anom_la',
                             filter_variable = 'diagnoses'))

  expect_error(evp_output(process_output = tbl_test,
                          output_level = 'test',
                          output_function = 'evp_ms_anom_la',
                          filter_variable = 'diagnoses'))
})
