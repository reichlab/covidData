## covidData 1.0.1

#### Feature updates
- 

#### package updates
- details on other changes will be listed here for future updates

### v 0.1
 - initial release: functionality for loading data for cases, deaths, and hospitalizations

#### v 0.1.1
 - fix bug where assemble-historical-healthdata.R tried to access data in the package,
  which resulted in errors for first-time package installation.

#### v 0.1.2
 - handle formatting inconsistencies in posting data to healthdata.gov
 - data posted on healthdata.gov on a certain date are no longer returned as part of issue dates before that date
 - refactor healthdata data processing

#### v 0.1.3
 - handle errors about SSL certificates expired when pulling data from HealthData.gov
 - handle the fact that HealthData.gov posted data with upload date of 12/21 and going through 12/28 (since corrected on their site).  We now manually force the issue date (based on the upload date) to be at least as large as the last date in the data file.

#### v 0.1.4
 - temporary fix to download only JHU data files from Sundays, Mondays, and the most recent week within the build date.

### v1.0

#### v 1.0.0
 - Update handling of JHU data: at time of package installation, we only download data for the most recent few days, as well as links to download other data files. Those other files are downloaded and processed only at the time of a request for them.
 - Looking at `covidData::jhu_deaths_data$issue_date` is no longer a reliable way to see all available issue dates!  This may break existing code.  Instead, use new helper function `available_issue_dates`.

#### v 1.0.1
 - import the readr package
