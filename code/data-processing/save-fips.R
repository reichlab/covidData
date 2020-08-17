## save fips codes as data object in package

fips_codes <- readr::read_csv("data-raw/locations.csv")

save(fips_codes, file = "data/fips_codes.rdata")
