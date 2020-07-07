## save fips codes as data object in package

fips_codes <- read_csv("data-raw/fips_codes.csv")

save(fips_codes, file = "data/fips_codes.rdata")
