# covidData
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5208224.svg)](https://doi.org/10.5281/zenodo.5208224)

This is an R package that provides versioned time series data for COVID-19 from the JHU data repository at https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series.

# Installation and Updating Data

For this package to be useful, you will need to clone it and install it from your local copy after you have updated the data.

To clone the repository, in a terminal you can run `git clone https://github.com/reichlab/covidData.git`

## installation prerequisites

You will need R 3.x or 4.x

## installation via `remotes`

You can install the package using the `remotes` package using

``` r
remotes::install_github('reichlab/covidData')
```

## manual installation
If installation using `remotes` does not work for you, you may run the following steps by hand.

1. Clone the repository. If you have previously installed covidData from an existing local clone, pull any updates to the package by running `git pull origin master` from the terminal, with the repository folder as your working directory.
1. Run `R CMD INSTALL covidData` from the terminal

## data disclaimer
*The names of locations included on the Website correspond with the official designations used by the U.S. Department of State. The presentation of material therein does not imply the expression of any opinion whatsoever on the part of JHU concerning the legal status of any country, area or territory or of its authorities. The depiction and use of boundaries, geographic names and related data shown on maps and included in lists, tables, documents, and databases on this website are not warranted to be error free nor do they necessarily imply official endorsement or acceptance by JHU.
