
#' Expected Variables Present
#'
#' This is a completeness module that will assess the presence of expected study variables and compute the distribution of
#' these variables in the dataset. The user will provide the variables (`evp_variable_file`) of interest, including the name of
#' the concept sets with the concepts used to define the variable. Sample versions of these inputs, both for OMOP and
#' PCORnet, are included as data in the package and are accessible with `expectedvariablespresent::`.
#' Results can optionally be stratified by site, age group, and/or time. This function is compatible with
#' both the OMOP and the PCORnet CDMs based on the user's selection.
#'
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#'    - `omop`: run the [evp_process_omop()] function against an OMOP CDM instance
#'    - `pcornet`: run the [evp_process_pcornet()] function against a PCORnet CDM instance
#'
#' @param evp_variable_file *tabular input* || **required**
#'
#'   A table with information about each of the variables that should be examined
#'   in the analysis. This table should contain the following columns:
#'   - `variable` | *character* | a string label for the variable captured by the associated codeset
#'   - `domain_tbl` | *character* | the CDM table where the variable is found
#'   - `concept_field` | *character* | the string name of the field in the domain table where the concepts are located
#'   - `date_field` | *character* | the name of the field in the domain table with the date that should be used for temporal filtering
#'   - `vocabulary_field` | *character* | for PCORnet applications, the name of the field in the domain table with a vocabulary identifier to differentiate concepts from one another (ex: dx_type); can be set to NA for OMOP applications
#'   - `codeset_name` | *character* | the name of the codeset that defines the variable of interest
#'   - `filter_logic` | *character* | logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a dplyr::filter command in R
#'
#'   To see an example of the structure of this file, please see `?expectedvariablespresent::evp_variable_file_omop` or
#'   `?expectedvariablespresent::evp_variable_file_pcornet`
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param output_level *string* || defaults to `patient`
#'
#'   A string indicating the analysis level to use as the basis for the
#'   Multi Site, Anomaly Detection computations
#'
#'   Acceptable values are either `patient` or `row`
#'
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param p_value *numeric* || defaults to `0.9`
#'
#'   The p value to be used as a threshold in the Multi-Site,
#'   Anomaly Detection, Cross-Sectional analysis
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @return This function will return a dataframe summarizing the
#'         distribution of each user-defined variable. For a
#'         more detailed description of output specific to each check type,
#'         see the PEDSpace metadata repository
#'
#' @example inst/example-evp_process_output.R
#'
#' @import cli
#'
#' @export
#'
evp_process <- function(cohort,
                        omop_or_pcornet,
                        evp_variable_file,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        output_level = 'patient',
                        age_groups = NULL,
                        p_value = 0.9,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'evp',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    evp_rslt <- evp_process_omop(cohort = cohort,
                                 evp_variable_file = evp_variable_file,
                                 multi_or_single_site = multi_or_single_site,
                                 anomaly_or_exploratory = anomaly_or_exploratory,
                                 output_level = output_level,
                                 age_groups = age_groups,
                                 time = time,
                                 time_period = time_period,
                                 time_span = time_span,
                                 p_value = p_value)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    evp_rslt <- evp_process_pcornet(cohort = cohort,
                                    evp_variable_file = evp_variable_file,
                                    multi_or_single_site = multi_or_single_site,
                                    anomaly_or_exploratory = anomaly_or_exploratory,
                                    output_level = output_level,
                                    age_groups = age_groups,
                                    time = time,
                                    time_period = time_period,
                                    time_span = time_span,
                                    p_value = p_value)
  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                    '`evp_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                    'See ?evp_output for more details.'), padding = c(0,1,0,1),
                  header = cli::col_cyan('Output Function Details')))

  return(evp_rslt %>% mutate(output_function = output_type$string))

}
