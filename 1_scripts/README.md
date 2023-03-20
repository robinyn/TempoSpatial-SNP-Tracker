# Preprocessing scripts

These scripts are used to preprocess the raw data and generate an SQL database for TSST to use. Please note that adding new data to TSST is currently not an officially supported feature of TSST. Disregard the scripts in the "test_scripts" folder.

## [parsePlink.py](parsePlink.py)

A python parser to generate three TSV files from the PLINK (.PED, .MAP, .GCOUNT) files and the sample metadata excel file. Look in the main README for more instructions.

Requires:
* Python (version 3.10.9)
* Pandas (version 1.5.3)

## [pairwise-geodetic.R](pairwise-geodetic.R)

An R script to generate a distance matrix between all samples using an SQL database containing sample IDs and their respective lat/lon coordinates. The script will output a TSV file.

Requires:
* R (version 4.2.1)
* RSQLite (version 2.3.0)
* Tidyverse (version 1.3.2)

## [giscoR_getCountry.R](giscoR_getCountry.R)

An R script to retrieve centroid coordinates for different countries from the Eurostat GISCO database and generate a TSV file.

Requires:
* R (version 4.2.1)
* sf (version 1.0-9)
* Tidyverse (version 1.3.2)