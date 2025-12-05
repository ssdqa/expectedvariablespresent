# Expected Variables Present

This is a completeness module that will assess the presence of expected
study variables and compute the distribution of these variables in the
dataset. The user will provide the variables (`evp_variable_file`) of
interest, including the name of the concept sets with the concepts used
to define the variable. Sample versions of these inputs, both for OMOP
and PCORnet, are included as data in the package and are accessible with
`expectedvariablespresent::`. Results can optionally be stratified by
site, age group, and/or time. This function is compatible with both the
OMOP and the PCORnet CDMs based on the user's selection.

## Usage

``` r
evp_process(
  cohort,
  omop_or_pcornet,
  evp_variable_file,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  output_level = "patient",
  age_groups = NULL,
  p_value = 0.9,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`evp_process_omop()`](https://ssdqa.github.io/expectedvariablespresent/reference/evp_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`evp_process_pcornet()`](https://ssdqa.github.io/expectedvariablespresent/reference/evp_process_pcornet.md)
    function against a PCORnet CDM instance

- evp_variable_file:

  *tabular input* \|\| **required**

  A table with information about each of the variables that should be
  examined in the analysis. This table should contain the following
  columns:

  - `variable` \| *character* \| a string label for the variable
    captured by the associated codeset

  - `domain_tbl` \| *character* \| the CDM table where the variable is
    found

  - `concept_field` \| *character* \| the string name of the field in
    the domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  - `codeset_name` \| *character* \| the name of the codeset that
    defines the variable of interest

  - `filter_logic` \| *character* \| logic to be applied to the
    domain_tbl in order to achieve the definition of interest; should be
    written as if you were applying it in a dplyr::filter command in R

  To see an example of the structure of this file, please see
  [`?expectedvariablespresent::evp_variable_file_omop`](https://ssdqa.github.io/expectedvariablespresent/reference/evp_variable_file_omop.md)
  or
  [`?expectedvariablespresent::evp_variable_file_pcornet`](https://ssdqa.github.io/expectedvariablespresent/reference/evp_variable_file_pcornet.md)

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- output_level:

  *string* \|\| defaults to `patient`

  A string indicating the analysis level to use as the basis for the
  Multi Site, Anomaly Detection computations

  Acceptable values are either `patient` or `row`

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- p_value:

  *numeric* \|\| defaults to `0.9`

  The p value to be used as a threshold in the Multi-Site, Anomaly
  Detection, Cross-Sectional analysis

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

## Value

This function will return a dataframe summarizing the distribution of
each user-defined variable. For a more detailed description of output
specific to each check type, see the PEDSpace metadata repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'expectedvariablespresent'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'evp_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Execute `evp_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
evp_process_example <- evp_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   evp_variable_file = evp_variable_file_omop) %>%
  suppressMessages()
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `evp_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output                               │
#> │ Required for Check: output_level                              │
#> │                                                               │
#> │ See ?evp_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

evp_process_example
#> # A tibble: 1 × 9
#>   site  total_pt_ct total_row_ct variable_pt_ct variable_row_ct prop_pt_variable
#>   <chr>       <int>        <int>          <int>           <int>            <dbl>
#> 1 comb…           7           70              5               5            0.714
#> # ℹ 3 more variables: prop_row_variable <dbl>, variable <chr>,
#> #   output_function <chr>

#' Execute `evp_output` function
evp_output_example <- evp_output(process_output = evp_process_example,
                                 output_level = 'patient')

evp_output_example


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(evp_output_example)

{"x":{"data":[{"orientation":"v","width":0.71399999999999997,"base":0.55000000000000004,"x":[0.35699999999999998],"y":[0.89999999999999991],"text":"prop_pt_variable: 0.714<br />variable: Sample OMOP Variable<br />variable: Sample OMOP Variable","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,77,111,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Sample OMOP Variable","legendgroup":"Sample OMOP Variable","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":52.529680365296812,"r":7.3059360730593621,"b":37.260273972602747,"l":142.46575342465755},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Proportion of Patients per Variable","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.035700000000000003,0.74969999999999992],"tickmode":"array","ticktext":["0.0","0.2","0.4","0.6"],"tickvals":[0,0.20000000000000004,0.40000000000000002,0.60000000000000009],"categoryorder":"array","categoryarray":["0.0","0.2","0.4","0.6"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"Proportion Patients","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-21.917808219178088},{"text":"Variable","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-127.12328767123289}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,1.6000000000000001],"tickmode":"array","ticktext":["Sample OMOP Variable"],"tickvals":[1],"categoryorder":"array","categoryarray":["Sample OMOP Variable"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1a636a517bda":{"x":{},"y":{},"fill":{},"type":"bar"}},"cur_data":"1a636a517bda","visdat":{"1a636a517bda":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
