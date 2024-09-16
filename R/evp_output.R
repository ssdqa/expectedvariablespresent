
#' EVP output function
#'
#' @param process_output the output of the `evp_process` function
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv
#'                        file that is output to the provided results folder after running the `evp_process` function
#' @param output_level the type of counts the output should summarise -- either `patient` or `row`
#' @param filter_variable for @ms_anom_at, @ms_exp_at, and @ss_anom_at, the single variable that should be displayed in the output;
#'                        can be any of the variables listed in the `evp_process` output
#' @param facet the variables by which you would like to facet the graph. available and/or recommended options for
#'              faceting variables are provided in the `parameter_summary` csv file
#'
#' @return a graph to visualize the results from `evp_process` based on the parameters provided
#'
#' @export
#'
evp_output <- function(process_output,
                       output_function,
                       output_level,
                       filter_variable,
                       facet = NULL){

  if(output_function == 'evp_ss_exp_nt'){

    evp_output <- evp_ss_exp_nt(process_output = process_output,
                                output_level = output_level,
                                facet = facet)

  }else if(output_function == 'evp_ss_anom_nt'){

    evp_output <- evp_ss_anom_nt(process_output = process_output,
                                 facet = facet)

  }else if(output_function == 'evp_ss_exp_at'){

    evp_output <- evp_ss_exp_at(process_output = process_output,
                                output_level = output_level,
                                facet = facet)

  }else if(output_function == 'evp_ss_anom_at'){

    evp_output <- evp_ss_anom_at(process_output = process_output,
                                 output_level = output_level,
                                 facet = facet,
                                 filter_variable = filter_variable)

  }else if(output_function == 'evp_ms_exp_nt'){

    evp_output <- evp_ms_exp_nt(process_output = process_output,
                                output_level = output_level,
                                facet = facet)

  }else if(output_function == 'evp_ms_anom_nt'){

    evp_output <- evp_ms_anom_nt(process_output = process_output,
                                 output_level = output_level)

  }else if(output_function == 'evp_ms_exp_at'){

    evp_output <- evp_ms_exp_at(process_output = process_output,
                                output_level = output_level,
                                filter_variable = filter_variable,
                                facet = facet)

  }else if(output_function == 'evp_ms_anom_at'){

    evp_output <- evp_ms_anom_at(process_output = process_output,
                                 output_level = output_level,
                                 filter_variable = filter_variable)

  }else(cli::cli_abort('Please enter a valid output function for this check type.'))

  return(evp_output)

}

