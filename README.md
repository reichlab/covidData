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
We recommend using `make` to simplify the updating of data. To do this, you must complete the following steps once you have cloned the repository:

1. In a terminal/shell window, navigate to `code/data-processing`.
2. Run `make all`. (This runs the steps below.)

Note: If you want to only download the most recent JHU data files, for step 2. instead run `make recent_data`

## manual installation
If make does not work for you, you may run the following steps by hand. To update the data, follow these steps once you have cloned the repository:

1. Create a `JHU` folder within `data-raw`; raw data files from the JHU repository will be stored here.
1. In a terminal, navigate to `code/data-processing` and run `python download-historical-jhu.py`; this downloads raw data files from the JHU repository.
1. From the same working directory, run `Rscript download-historical-healthdata.R`
1. In R, with your working directory set to the package root, run the contents of the `assemble-historical-jhu.R` and `assemble-historical-healthdata.R` scripts in `code/data-processing`.
1. Whenever you have done this, you will need to re-install the package.  To avoid unnecessary duplication and copying of files, we recommend installing the package from the terminal by `R CMD INSTALL covidData`.

Note: If you want to only download the most recent JHU data files, for step 2. instead run `python download-historical-jhu.py --recent`