
#' Expected Variables Present
#'
#' This is a completeness module that will assess the presence of expected study variables and compute the distribution of
#' these variables in the dataset. The user will provide the variables (`evp_variable_file`) of interest, including the name of
#' the concept sets with the concepts used to define the variable. Sample versions of these inputs, both for OMOP and
#' PCORnet, are included as data in the package and are accessible with `expectedvariablespresent::`.
#' Results can optionally be stratified by site, age group, and/or time. This function is compatible with
#' both the OMOP and the PCORnet CDMs based on the user's selection.
#'
#' @param cohort *tabular input* | A dataframe with the cohort of patients for your study. Should include the columns:
#' - `person_id` / `patid` | *integer* / *character*
#' - `start_date` | *date*
#' - `end_date` | *date*
#' - `site` | *character*
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [evp_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [evp_process_pcornet()] function against a PCORnet CDM instance
#' @param evp_variable_file *tabular input* | CSV file with information about each of the variables that should be
#' examined in the function. contains the following columns:
#' - `variable` | *character* | a label for the variable captured by the associated codeset
#' - `default_tbl` | *character* | CDM table where data related to the codeset is found
#' - `concept_field` | *character* | concept_id field with codes from the associated codeset
#' - `date_field` | *character* | a date field in the `default_tbl` that should be used for over time analyses
#' - `vocabulary_field` | *character* | PCORNET ONLY; field in the `default_tbl` that defines the vocabulary type of the concept (i.e. dx_type)
#' if this field is used, the codeset should have a `vocabulary_id` column that defines the appropriate vocabularies for each concept
#' - `codeset_name` | *character* | the name of the codeset file; DO NOT include the file extension
#' - `filter_logic` | *character* | a string indicating filter logic that should be applied to achieve the desired variable; optional
#' @param multi_or_single_site *string* | Option to run the function on a single vs multiple sites
#' - `single`: run the function for a single site
#' - `multi`: run the function for multiple sites
#' @param anomaly_or_exploratory *string* | Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#' level summary of the data to examine the fact representation within the cohort. Anomaly detection
#' analyses are specialized to identify outliers within the cohort.
#' @param output_level *string* | the level of output to use for an euclidean distance computation, exclusive to `ms_anom_la`;
#' either `patient` or `row` -- defaults to `row`
#' @param age_groups *tabular input* | If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#' - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#' - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param p_value *numeric* | the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param time *boolean* | a logical that tells the function whether you would like to look at the output over time
#' @param time_span *vector - length 2* | when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param time_period *string* | when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe with patient/row counts & proportions for each concept set listed in `evp_variable_file`.
#'         this output should then be used in the `evp_output` function to generate an appropriate
#'         visualization
#'
#' @example inst/example-evp_process_output.R
#'
#' @import cli
#'
#' @export
#'
evp_process <- function(cohort,
                        omop_or_pcornet = 'omop',
                        evp_variable_file,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        output_level = 'row',
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
