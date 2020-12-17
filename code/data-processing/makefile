# covidData

# Define global variables
# Where JHU files are downloaded
JHUDIR = ../../data-raw/JHU/
# One directory above package root
ROOTDIR = ../../../

# Define the full make
all: install assemble download

# Install R package with updated data
install: assemble download
	R CMD INSTALL ../../../covidData

# Assemble csv files into R data object
assemble: download
	Rscript assemble-historical-jhu.R
	Rscript assemble-historical-healthdata.R

# Download data from JHU CSSE GitHub repo
download:
	python3 download-historical-jhu.py
	Rscript download-historical-healthdata.R
