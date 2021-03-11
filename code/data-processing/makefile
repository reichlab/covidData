# covidData

# Define global variables
# Where JHU files are downloaded
JHUDIR = ../../data-raw/JHU/
# One directory above package root
ROOTDIR = ../../../

# Define the full make
all: install assemble download

# Define the full make using only recent data
recent_data: install_recent assemble_recent download_recent

# Install R package with updated data
install: assemble download
	R CMD INSTALL ../../../covidData

# Assemble csv files into R data object
assemble: download
	Rscript assemble-historical-jhu.R
	Rscript assemble-historical-healthdata.R

# Download data from JHU CSSE GitHub repo
download:
	Rscript download-historical-jhu.R
	Rscript download-historical-healthdata.R

# Install R package with most recent data
install_recent: assemble_recent download_recent
	R CMD INSTALL ../../../covidData

# Assemble recent csv files into R data object
assemble_recent: download_recent
	Rscript assemble-historical-jhu.R
	Rscript assemble-historical-healthdata.R

# Download most recent data from JHU CSSE GitHub repo
download_recent:
	python3 download-historical-jhu.py --recent
	Rscript download-historical-healthdata.R