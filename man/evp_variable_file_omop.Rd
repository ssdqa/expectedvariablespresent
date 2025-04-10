% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{evp_variable_file_omop}
\alias{evp_variable_file_omop}
\title{EVP Sample Variable File -- OMOP}
\format{
\subsection{\code{evp_variable_file_omop}}{

A data frame with 6 columns
\describe{
\item{variable}{A string label for the variable of interest}
\item{domain_tbl}{The name of the CDM table where the variable can be found}
\item{concept_field}{The field in the default_tbl that should be used to join to the codeset}
\item{date_field}{The date field in the default_tbl that should be used to filter the dataset to the cohort period and for longitudinal analyses}
\item{codeset_name}{The name of the codeset as found in the specs directory; file extension should not be included}
\item{filter_logic}{optional; a string to be parsed as logic to filter the default_tbl and better identify the variable of interest}
}
}
}
\usage{
evp_variable_file_omop
}
\description{
A sample version of the file structure expected for the evp_variable_file
parameter in the \code{evp_process} function. The user should recreate this file
structure and include their own variables.
}
\details{
Please note that the codesets should be stored in the \code{file_subdirectory} established
when \code{ssdqa.gen::initialize_dq_session} is executed.

Examples of appropriately structured codeset files are attached to the ssdqa.gen
package and can be accessed with \verb{ssdqa.gen::}
}
\keyword{datasets}
