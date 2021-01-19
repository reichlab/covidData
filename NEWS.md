## covidData 0.1.4

This is the first version of the package with a 0.x release.

### Feature updates
- details on new features will be listed here for future updates
- current key features include `load_data` function to load versioned counts of cases, deaths, and hospitalizations due to COVID-19

### package updates
- pre process the data from JHU (cases and deaths) to be in long format and store incidence cases/deaths over cumulative

## v 0.1
 - initial release: functionality for loading data for cases, deaths, and hospitalizations

### v 0.1.1
 - fix bug where assemble-historical-healthdata.R tried to access data in the package,
  which resulted in errors for first-time package installation.

### v 0.1.2
 - handle formatting inconsistencies in posting data to healthdata.gov
 - data posted on healthdata.gov on a certain date are no longer returned as part of issue dates before that date
 - refactor healthdata data processing

### v 0.1.3
 - handle errors about SSL certificates expired when pulling data from HealthData.gov
 - handle the fact that HealthData.gov posted data with upload date of 12/21 and going through 12/28 (since corrected on their site).  We now manually force the issue date (based on the upload date) to be at least as large as the last date in the data file.
 
### v 0.1.4
 - create `load_data` function to replace functions `load_jhu_data` and `load_healthdata_data`