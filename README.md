# covidData

This is an R package that provides versioned time series data for COVID-19 from the JHU data repository at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series.

# Installation and Updating Data

For this package to be useful, you will probably want to clone it and install it from your local copy after you have updated the data.

To update the data, follow these steps once you have cloned the repository:

1. Create a `JHU` folder within `data-raw`; raw data files from the JHU repository will be stored here.
2. In a terminal, navigate to `code/data-processing` and run `python download-historical-jhu.py`; this downloads raw data files from the JHU repository.
3. In R, with your working directory set to the package root, run the contents of the `assemble-historical-jhu.R` script in `code/data-processing`.
4. Whenever you have done this, you will need to re-install the package.  This can be done within R by `devtools::install()` or from the terminal by `R CMD INSTALL covidData`.
