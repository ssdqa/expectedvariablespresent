% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evp_process.R
\name{evp_process}
\alias{evp_process}
\title{Expected Variables Present}
\usage{
evp_process(
  cohort,
  omop_or_pcornet = "omop",
  evp_variable_file,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  output_level = "row",
  age_groups = NULL,
  p_value = 0.9,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
}
\arguments{
\item{cohort}{\emph{tabular input} | A dataframe with the cohort of patients for your study. Should include the columns:
\itemize{
\item \code{person_id} / \code{patid} | \emph{integer} / \emph{character}
\item \code{start_date} | \emph{date}
\item \code{end_date} | \emph{date}
\item \code{site} | \emph{character}
}}

\item{omop_or_pcornet}{\emph{string} | Option to run the function using the OMOP or PCORnet CDM as the default CDM
\itemize{
\item \code{omop}: run the \code{\link[=evp_process_omop]{evp_process_omop()}} function against an OMOP CDM instance
\item \code{pcornet}: run the \code{\link[=evp_process_pcornet]{evp_process_pcornet()}} function against a PCORnet CDM instance
}}

\item{evp_variable_file}{\emph{tabular input} | CSV file with information about each of the variables that should be
examined in the function. contains the following columns:
\itemize{
\item \code{variable} | \emph{character} | a label for the variable captured by the associated codeset
\item \code{default_tbl} | \emph{character} | CDM table where data related to the codeset is found
\item \code{concept_field} | \emph{character} | concept_id field with codes from the associated codeset
\item \code{date_field} | \emph{character} | a date field in the \code{default_tbl} that should be used for over time analyses
\item \code{vocabulary_field} | \emph{character} | PCORNET ONLY; field in the \code{default_tbl} that defines the vocabulary type of the concept (i.e. dx_type)
if this field is used, the codeset should have a \code{vocabulary_id} column that defines the appropriate vocabularies for each concept
\item \code{codeset_name} | \emph{character} | the name of the codeset file; DO NOT include the file extension
\item \code{filter_logic} | \emph{character} | a string indicating filter logic that should be applied to achieve the desired variable; optional
}}

\item{multi_or_single_site}{\emph{string} | Option to run the function on a single vs multiple sites
\itemize{
\item \code{single}: run the function for a single site
\item \code{multi}: run the function for multiple sites
}}

\item{anomaly_or_exploratory}{\emph{string} | Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
level summary of the data to examine the fact representation within the cohort. Anomaly detection
analyses are specialized to identify outliers within the cohort.}

\item{output_level}{\emph{string} | the level of output to use for an euclidean distance computation, exclusive to \code{ms_anom_la};
either \code{patient} or \code{row} -- defaults to \code{row}}

\item{age_groups}{\emph{tabular input} | If you would like to stratify the results by age group,  create a table or CSV file with the following
columns and include it as the \code{age_groups} function parameter:
\itemize{
\item \code{min_age} | \emph{integer} | the minimum age for the group (i.e. 10)
\item \code{max_age} | \emph{integer} | the maximum age for the group (i.e. 20)
\item \code{group} | \emph{character} | a string label for the group (i.e. 10-20, Young Adult, etc.)
}

If you would \emph{not} like to stratify by age group, leave the argument as NULL}

\item{p_value}{\emph{numeric} | the p value to be used as a threshold in the multi-site anomaly detection analysis}

\item{time}{\emph{boolean} | a logical that tells the function whether you would like to look at the output over time}

\item{time_span}{\emph{vector - length 2} | when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
formatted as c(start date, end date) in yyyy-mm-dd date format}

\item{time_period}{\emph{string} | when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
to \code{year}, but other time periods such as \code{month} or \code{week} are also acceptable}
}
\value{
a dataframe with patient/row counts & proportions for each concept set listed in \code{evp_variable_file}.
this output should then be used in the \code{evp_output} function to generate an appropriate
visualization
}
\description{
This is a completeness module that will assess the presence of expected study variables and compute the distribution of
these variables in the dataset. The user will provide the variables (\code{evp_variable_file}) of interest, including the name of
the concept sets with the concepts used to define the variable. Sample versions of these inputs, both for OMOP and
PCORnet, are included as data in the package and are accessible with \verb{expectedvariablespresent::}.
Results can optionally be stratified by site, age group, and/or time. This function is compatible with
both the OMOP and the PCORnet CDMs based on the user's selection.
}
\examples{

#' Source setup file
source(system.file('setup.R', package = 'expectedvariablespresent'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'evp_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'expectedvariablespresent'),
                      cdm_schema = NA)

#' Build mock study cohort
cohort <- cdm_tbl('person') \%>\% dplyr::distinct(person_id) \%>\%
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id \%in\% c(1:6), 'synth1', 'synth2'))

#' Execute `evp_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
evp_process_example <- evp_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   evp_variable_file = evp_variable_file_omop)

evp_process_example

#' Execute `evp_output` function
evp_output_example <- evp_output(process_output = evp_process_example,
                                 output_level = 'patient')

evp_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(evp_output_example)
}
