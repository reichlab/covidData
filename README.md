# covidData

This is an R package that provides versioned time series data for COVID-19 from the JHU data repository at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series.

# Installation and Updating Data

For this package to be useful, you will need to clone it and install it from your local copy after you have updated the data.

To clone the repository, in a terminal you can run `git clone https://github.com/reichlab/covidData.git`

## installation prerequisites

You will need Python 3 and R 3.x or 4.x, as well as the packages listed below:

Python packages: `pandas` and `requests`. To install, run the following in a terminal:

```
pip3 install pandas
pip3 install requests
```

## installation using `make`
We recommend using `make` to simplify the updating of data. However, some Windows operating systems do not allow for easy use of the command `make`, so manual installation should be used if `make` does not work. 

To use `make`, you must complete the following steps once you have cloned the repository:

1. In a terminal/shell window, navigate to `code/data-processing`.
2. Run `make all`. (This runs the steps below.)

Note: If you want to only download the most recent JHU data files, for step 2. instead run `make recent_data`

## manual installation
If make does not work for you, you may run the following steps by hand. To update the data, follow these steps once you have cloned the repository:

1. Create a `JHU` folder within `data-raw`; raw data files from the JHU repository will be stored here.
1. In a terminal, navigate to `code/data-processing` and run `Rscript download-historical-healthdata.R`; this downloads raw data files for hospitalizations from HealthData.gov.
1. In R, with your working directory set to the package root, run the contents of the `assemble-historical-jhu.R` and `assemble-historical-healthdata.R` scripts in `code/data-processing`.
1. Whenever you have done this, you will need to re-install the package.  To avoid unnecessary duplication and copying of files, we recommend installing the package from the terminal by `R CMD INSTALL covidData`.

Note: If you want to only download the most recent JHU data files, for step 2. instead run `Rscript download-historical-jhu.R TRUE`

## data disclaimer
*The names of locations included on the Website correspond with the official designations used by the U.S. Department of State. The presentation of material therein does not imply the expression of any opinion whatsoever on the part of JHU concerning the legal status of any country, area or territory or of its authorities. The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance by JHU.
