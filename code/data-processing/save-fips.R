## save fips codes as data object in package
library(dplyr)
library(readr)

fips_codes <- read_csv("data-raw/locations.csv") %>%
  # Extract which state each county belongs to
  dplyr::mutate(
    state_code = ifelse(
      nchar(location) > 2,
      substr(location, 1, 2),
      location
    )
  ) %>%
  # For all locations, add a state abbreviation
  dplyr::left_join(
    covidData::fips_codes %>%
      dplyr::filter(nchar(location) == 2) %>%
      dplyr::select(state_code = location, state_abbr = abbreviation),
    by = 'state_code'
  ) %>%
  # Create "county, state" format for county names
  dplyr::mutate(
    location_name_with_state = ifelse(
      nchar(location) > 2,
      paste0(location_name, ', ', state_abbr),
      location_name
    )
  ) %>%
  # Drop extra columns we created above
  dplyr::select(location, location_name, location_name_with_state, abbreviation)

save(fips_codes, file = "data/fips_codes.rdata")
