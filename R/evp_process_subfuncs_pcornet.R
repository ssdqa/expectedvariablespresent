

#' Compute variable distribution (PCORnet)
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param time logical to determine whether the function is being run as part of `compute_fot` or not
#' @param evp_variable_file CSV file with information about each of the variables that should be
#' examined in the function. contains the following columns:
#' - `variable` a label for the variable captured by the associated codeset
#' - `default_tbl` CDM table where data related to the codeset is found
#' - `concept_field` concept_id field with codes from the associated codeset
#' - `date_field` a date field in the `default_tbl` that should be used for over time analyses
#' - `vocabulary_field` PCORNET ONLY; field in the `default_tbl` that defines the vocabulary type of the concept (i.e. dx_type)
#' if this field is used, the codeset should have a `vocabulary_id` column that defines the appropriate vocabularies for each concept
#' - `codeset_name` the name of the codeset file; DO NOT include the file extension
#' - `filter_logic` a string indicating filter logic that should be applied to achieve the desired variable; optional
#'
#' @return dataframe with patient/row counts and proportions that are computed per group defined in
#'         grouped_list, and if time = TRUE, for each time period defined in compute_fot
#'
#' @keywords internal
#'
compute_evp_pcnt <- function(cohort,
                             grouped_list,
                             time = FALSE,
                             evp_variable_file = expectedvariablespresent::evp_variable_file_pcornet){

  evp_list <- split(evp_variable_file, seq(nrow(evp_variable_file)))

  result <- list()

  for(i in 1:length(evp_list)){

    variable <- evp_list[[i]]$variable

    message(paste0('Starting ', variable))

    domain_tbl <- cdm_tbl(evp_list[[i]]$domain_tbl) %>%
      inner_join(cohort) %>%
      filter(!!sym(evp_list[[i]]$date_field) >= start_date &
               !!sym(evp_list[[i]]$date_field) <= end_date) %>%
      group_by(!!!syms(grouped_list))

    if(time){
      domain_tbl <- domain_tbl %>%
        filter(!!sym(evp_list[[i]]$date_field) >= time_start &
                 !!sym(evp_list[[i]]$date_field) <= time_end) %>%
        group_by(time_start, time_increment, .add = TRUE)
    }

    total_pts <- cohort %>%
      summarise(total_pt_ct = n_distinct(patid)) %>%
      collect()

    total_rows <- domain_tbl %>%
      summarise(total_row_ct = n()) %>%
      collect()

    totals <- total_rows %>%
      left_join(total_pts)

    join_cols <- purrr::set_names('concept_code', evp_list[[i]]$concept_field)

    vocab_col <- evp_list[[i]]$vocabulary_field

    if(!is.na(vocab_col)){

      join_cols2 <- set_names('vocabulary_id', vocab_col)
      join_cols <- join_cols %>% append(join_cols2)

    }

    if(is.na(evp_list[[i]]$filter_logic)){
      fact_pts <- domain_tbl %>%
        inner_join(load_codeset(evp_list[[i]]$codeset_name), by = join_cols) %>%
        summarise(variable_pt_ct = n_distinct(patid),
                  variable_row_ct = n()) %>% collect()
    }else{
      fact_pts <- domain_tbl %>%
        inner_join(load_codeset(evp_list[[i]]$codeset_name), by = join_cols) %>%
        filter(!! rlang::parse_expr(evp_list[[i]]$filter_logic)) %>%
        summarise(variable_pt_ct = n_distinct(patid),
                  variable_row_ct = n()) %>% collect()
    }

    if(nrow(fact_pts) < 1){
      if(!time){
      fact_pts <- tibble('variable_pt_ct' = 0,
                         'variable_row_ct' = 0,
                         'site' = totals$site)
      }else{
        fact_pts <- tibble('variable_pt_ct' = 0,
                           'variable_row_ct' = 0,
                           'site' = totals$site,
                           'time_start' = totals$time_start,
                           'time_increment' = totals$time_increment)
      }
    }

    final_tbl <- totals %>%
      left_join(fact_pts) %>%
      mutate(prop_pt_variable = round(as.numeric(variable_pt_ct/total_pt_ct), 3),
             prop_row_variable = round(as.numeric(variable_row_ct/total_row_ct), 3),
             variable = variable)

    final_tbl[is.na(final_tbl)] <- 0

    result[[paste0(evp_list[[i]]$variable)]] <- final_tbl
  }

  compress <- reduce(.x = result,
                     .f = dplyr::union)

  return(compress)


}



#' Single site anomaly no time processing for EVP
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param evp_variable_file CSV file with information about each of the variables that should be
#' examined in the function. contains the following columns:
#' - `variable` a label for the variable captured by the associated codeset
#' - `default_tbl` CDM table where data related to the codeset is found
#' - `concept_field` concept_id field with codes from the associated codeset
#' - `date_field` a date field in the `default_tbl` that should be used for over time analyses
#' - `vocabulary_field` PCORNET ONLY; field in the `default_tbl` that defines the vocabulary type of the concept (i.e. dx_type)
#' if this field is used, the codeset should have a `vocabulary_id` column that defines the appropriate vocabularies for each concept
#' - `codeset_name` the name of the codeset file; DO NOT include the file extension
#' - `filter_logic` a string indicating filter logic that should be applied to achieve the desired variable; optional
#'
#' @return one dataframe with the jaccard similarity index for each concept group provided
#'         in the concept file
#'
#' @keywords internal
#'
compute_evp_ssanom_pcnt <- function(cohort,
                                    grouped_list,
                                    evp_variable_file = expectedvariablespresent::evp_variable_file_pcornet){

  evp_list <- split(evp_variable_file, seq(nrow(evp_variable_file)))

  result <- list()

  for(i in 1:length(evp_list)){

    variable <- evp_list[[i]]$variable

    join_cols <- purrr::set_names('concept_code', evp_list[[i]]$concept_field)

    vocab_col <- evp_list[[i]]$vocabulary_field

    if(!is.na(vocab_col)){

      join_cols2 <- set_names('vocabulary_id', vocab_col)
      join_cols <- join_cols %>% append(join_cols2)

    }

    if(is.na(evp_list[[i]]$filter_logic)){
      domain_tbl <- cdm_tbl(evp_list[[i]]$domain_tbl) %>%
        inner_join(cohort) %>%
        filter(!!sym(evp_list[[i]]$date_field) >= start_date &
                 !!sym(evp_list[[i]]$date_field) <= end_date) %>%
        inner_join(load_codeset(evp_list[[i]]$codeset_name), by = join_cols) %>%
        group_by(!!!syms(grouped_list)) %>%
        mutate(variable = variable) %>%
        select(patid,
               all_of(group_vars(cohort)),
               variable) %>%
        group_by(patid, variable, .add = TRUE) %>%
        summarise(ct = n())

    }else{
      domain_tbl <- cdm_tbl(evp_list[[i]]$domain_tbl) %>%
        inner_join(cohort) %>%
        filter(!!sym(evp_list[[i]]$date_field) >= start_date &
                 !!sym(evp_list[[i]]$date_field) <= end_date) %>%
        filter(!! rlang::parse_expr(evp_list[[i]]$filter_logic)) %>%
        inner_join(load_codeset(evp_list[[i]]$codeset_name), by = join_cols) %>%
        group_by(!!!syms(grouped_list)) %>%
        mutate(variable = variable) %>%
        select(patid,
               all_of(group_vars(cohort)),
               variable) %>%
        group_by(patid, variable, .add = TRUE) %>%
        summarise(ct = n())
    }

    result[[i]] <- domain_tbl

  }

  domain_reduce <- purrr::reduce(.x = result,
                                 .f = dplyr::union) %>%
    collect() %>%
    unite(facet_col, !!!syms(grouped_list), sep = '\n')

  facet_list <- group_split(domain_reduce %>% group_by(facet_col))

  jacc_list <- list()

  for(i in 1:length(facet_list)){

    grp <- facet_list[[i]] %>% distinct(facet_col) %>% pull()

    jaccards <- compute_jaccard(jaccard_input_tbl = facet_list[[i]],
                                var_col = 'variable',
                                omop_or_pcornet = 'pcornet') %>%
      mutate(grp = grp)

    jacc_list[[i]] <- jaccards

  }

  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)

  return(jacc_reduce)
}
