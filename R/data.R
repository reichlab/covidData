#' FIPS codes for the US and its states and territories
#'
#' @format A data.frame with two-digit or five-digit FIPS codes, location names,
#' and two-letter abbreviations of states and territories of the US
"fips_codes"


#' JHU data for the US and its states and territories with counts of deaths
#'
#' @format A tibble with a row per issue date
"jhu_deaths_data"


#' JHU data for the US and its states and territories with counts of cases
#'
#' @format A tibble with a row per issue date
"jhu_cases_data"

#' HealthData.gov data for the US and its states and territories with counts of
#' hospitalizations
#'
#' @format A tibble with a row per issue date
"healthdata_hosp_data"


#' Imputed death incidence data for negative incidence and adjustment cases
#'
#' @format A data.frame with location (fips code), date, 
#' measure ('deaths') and inc
"jhu_deaths_imputed_data"

#' imputed cases incidence data for negative incidence and adjustment cases
#'
#' @format A data.frame with location (fips code), date,
#' measure ('cases') and inc
"jhu_cases_imputed_data"

