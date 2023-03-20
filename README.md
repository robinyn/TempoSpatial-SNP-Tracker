# TempoSpatial SNP Tracker

## Table of contents

- [TempoSpatial SNP Tracker](#tempospatial-snp-tracker)
  - [Table of contents](#table-of-contents)
  - [1. Introduction](#1-introduction)
  - [2. Web version](#2-web-version)
  - [3. Installation](#3-installation)
    - [3.1. Required packages](#31-required-packages)
      - [**3.1.1. Conda**](#311-conda)
      - [**3.1.2. R console**](#312-r-console)
    - [3.2. Application files](#32-application-files)
  - [4. Running the application](#4-running-the-application)
  - [5. Using the application](#5-using-the-application)
  - [6. Inputting new data](#6-inputting-new-data)
    - [6.1. Prepping PLINK files](#61-prepping-plink-files)
    - [6.2. Prepping the sample metadata file](#62-prepping-the-sample-metadata-file)
    - [6.3. Parsing](#63-parsing)
    - [6.4. Generate SQL database](#64-generate-sql-database)
    - [6.5. Generate distance matrix](#65-generate-distance-matrix)
    - [6.6. Get country centroid coordinates](#66-get-country-centroid-coordinates)
    - [6.7. Import distance matrix to database](#67-import-distance-matrix-to-database)
    - [6.8. Edit global.R](#68-edit-globalr)

## 1. Introduction

TempoSpatial SNP Tracker (TSST) is a tool aimed at simplifying the visualization of large SNP datasets with both temporal and geospatial components. TSST will produce an interactive map with a timeline and plots of their choice visualizing the loaded SNP data, allowing users to easily flip through the entire time range of their data. With an intuitive and modern UI, users can flexibily subset and cluster their data. Extensive graphical options allow users to fully customize the resulting map to their needs. TSST is also capable of producing summary plots displaying the global allele frequencies/counts for the chosen SNPs over time. Finally, the data explorer allows users to easily search and examine the raw data.

## 2. Web version

The application is available at the following [link](https://robinyn.shinyapps.io/tsst/).

**Note:** Due to the limitations of the free hosting service provided by Shinyapps.io, the performance of the web version is rather poor. Please allow the application to have plenty of time to render the map after changing any of the options. Downloading and running the application will result in better performance, but there may be compatibility issues.

## 3. Installation

These steps are ***not necessary*** if you decide to run the web version of the application available in the link above. Please only follow these instructions if you want to run the application on your local machine.

### 3.1. Required packages

The application is written in R (version 4.2.1) and the following packages are required:

* R Shiny (v1.7.4)
* Shinywidgets (v0.7.6)
* Tidyverse (v1.3.2)
* R Leaflet (v2.1.1)
* Leaflet.minicharts (v0.6.2)
* RSQLite (v2.3.0)
* RColorBrewer (v1.1_3)
* Plotly (v4.10.1)

#### **3.1.1. Conda**

The recommended method of installing all of the required packages is through Conda (version 23.1.0). The following code can be run verbatim on the terminal/powershell.

```shell
# Create a conda environment with all of the necessary packages
conda create -n TSST -c bioconda -c conda-forge \
r-base=4.2.1                \
r-shiny=1.7.4               \
r-shinywidgets=0.7.6        \
r-tidyverse=1.3.2           \
r-leaflet=2.1.1             \
r-leaflet.minicharts=0.6.2  \
r-rsqlite=2.3.0             \
r-rcolorbrewer=1.1_3        \
r-plotly=4.10.1

# Activate the environment created above
conda activate TSST

# Run R console
R
```

Should any of the packages fail to install through Conda, try installing through the **R console** using the following line:

```R
install.packages("Name_of_the_package")
```

#### **3.1.2. R console**

The following commands can be used in R console/R studio to install the required packages manually.

```R
# Create a vector of packages to install
packages = c("shiny", "shinywidgets", "tidyverse", "leaflet", "leaflet.minicharts", "rsqlite", "rcolorbrewer", "plotly")

# Loop over and install required packages
for(package in packages){ install.packages(package) }
```

### 3.2. Application files

While in the future, we wish to support running TSST directly from this GitHub repository, due to the large filesize of the SQL database, it is not possible to do so at the moment. In order to run this application, you have to download the following files from this repo:

* **[global.R](global.R)**
* **[server.R](server.R)**
* **[ui.R](ui.R)**
* **[styles.css](styles.css)**
* **[reich_v50.sqlite](data/reich_v50.sqlite)**

You can put the downloaded files into the root directory where you want the application to run from, but the **reich_v50.sqlite** file must be within a folder named **data** inside the root directory. The resulting folder structure must look like the following:

- root_directory
  - data
    - reich_v50.sqlite
  - global.R
  - server.R
  - ui.R
  - styles.css

## 4. Running the application

Once all of the necessary files and packages have been downloaded and installed, the following line can be run in the R console to start the program.

```R
# Replace the example path with the actual path name to the root directory for the app

shiny::runApp("Absolute/Path/To/Root/Directory", launch.browser=TRUE)
```

If the following lines appear after running the command and no new window opens, copy the address and manually open the application in your browser of choice.

```shell
Listening on http://127.0.0.1:3964
Browsing http://127.0.0.1:3964
```

## 5. Using the application

The first page when running the application is the **Interactive Map** page with a panel on the left hand side called the **Controls** panel. This is where all of the options for subsetting, clustering, and visualizing the data can be selected and modified. The application is relatively intuitive. However, the following section explains some of the parameters users can modify.

| Parameter | Explanation | Options |
| --- | --- | --- |
| No. of SNPs | Number of SNPs to track | 1 / 2 |
| Plot type | Type of chart to plot on the map | Pie chart / Bar chart |
| Data type | Type of data to plot | Count / Frequency |
| Chromosome | Chromosome number where the SNP to track is located | Depends on data |
| Show labels | Toggle to show or hide labels of clusters | On / Off |
| Show missing data | Toggle to account for samples that have no genotyping information for a given SNP | On / Off |
| Grouping variables | Variable to cluster the samples by | None / Country / Distance |
| *Distance* | Threshold in kilometers for clustering samples | 0~5000 km|
| Time step | Time in years to move in each iteration | 100~5000 years |
| Window size | Window of time in years to use when subsetting samples | 100~5000 years |
| Timeline | Timeline to select the "starting time" for the window (the application will take the time selected here and the window size to calculate the time range for subsetting the samples) | Depends on data |
| Palettes | RColorBrewer palette type to use | Qualitative / Sequential / Divergent |
| Colors | RColorBrewewr palette to use | [link](https://r-graph-gallery.com/38-rcolorbrewers-palettes.html) |
| Legend | Toggle and change location of the legend | Off / Top right(TR) / Top left (TL) / Bottom right (BR) / Bottom left (BL) |
| Opactiy | Control how transparent the plots will be | 0~100% |
| Size | Control the size of the plots | 1~100% |

The REF/ALT markers next to the chromosome/SNP selection options represent the major and minor alleles for the selected SNP based on the entire dataset.

The **Summary** tab next to the controls panel will automatically generate summary plots based on the options selected in the controls panel. There are no options to modify the graphical options of the plots generated in the current version, but will be added in future updates.

The **Data Explorer** can be used to easily navigate through and examine the raw data loaded into the application. Modifying the various options on the top panel will filter the data to be displayed on the data table below.

## 6. Inputting new data

Unfortunately, the current version of TSST does not officially support adding new data other than the dataset preloaded in the application. However, it is possible to generate your own SQLite database file using the scripts available in this repo.

**Disclaimer: This is not an officially supported function of TSST at the moment. Please not that the scripts may not function as intended when using your own data.**

### 6.1. Prepping PLINK files

Uncompressed (.map/.ped) PLINK files are required for the parser. Please use the **--recode compound-genotypes"** flag in PLINK to generate PED files in compound genotype format.

```shell
# Generate .map and .ped files from .fam .bim .bed files
plink2 --bfile filename --recode --out output_name --recode compound-genotypes
```

Use PLINK again to generate a ***.gcount*** file with the major/minor allele information for all SNPs in the dataset.

```shell
# Generate .gcount files
plink2 -bfile filename -out output_name --geno-counts
```

### 6.2. Prepping the sample metadata file

An excel (.xlsx) file with the sample metadata is also required for the parser. The excel file must contain the following columns with the exact column names:

| Column name | Description |
| --- | --- |
| MasterID | This column should contain the sample IDs also present in the second column of the PED file. Every sample in the PED file should have a matching entry in this excel file with the matching sample ID |
| DateMean | This column should contain the temporal component of the data. The only supported format at the moment is BP years (Before Present).|
| Lat | The latitude coordinate where the sample was found |
| Long | The longitude coordinate where the sample was found |
| Country | The country where the sample was found |

The excel file may contain additional columns, but the columns listed above are **required** for the parser and application to function properly.

### 6.3. Parsing

Use the [parser](1_scripts/parsePlink.py) to generate three tab-separated files: ***Sample_Metadata.tsv***, ***samples.tsv***, ***SNP_list.tsv***. The parser requires Python (version 3.10.9) and pandas (version 1.5.3).

```shell
# Run the parser
python3 parsePlink.py
```

You will be promted to enter the path to all of the files required for the parser. The parser does not verify the inputs in any way, so please ensure that the paths are correct.

### 6.4. Generate SQL database

Use SQLite to import the three TSV files generated in the previous step. Please use the same names for the tables as listed here. The following lines must be run in the directory where the three TSV files from the previous step are located.

```shell
# Start SQLite console by creating a new database file
sqlite3 database_name.sqlite

# Run on SQLite console
.mode tabs
.import  Sample_Metadata.tsv sample_meta
.import samples.tsv main
.import SNP_list.tsv SNP_meta
```

### 6.5. Generate distance matrix

Now, the [pairwise-geodetic.R](1_scripts/pairwise-geodetic.R) script can be used to generate a distance matrix between all of the samples present in the data.

```shell
Rscript pairwise-geodetic.R "Directory/to/database/file"
```

The script will not verify the directory provided in any way, so please ensure that the correct path is provided.

Running the script will produce ***distanceMatrix.tsv***.

### 6.6. Get country centroid coordinates

You can download the ***[country_cords.tsv](data/country_cords.tsv)*** file from this repository, or use the [giscoR_getCountry.R](1_scripts/giscoR_getCountry.R) script to generate your own file.

Running this script requires the following additional package from the ones installed previously:
* sf (v1.0-9)

```shell
# Run the line on terminal
Rscript giscoR_getCountry.R
```

### 6.7. Import distance matrix to database

Use SQLite again to import the two new TSV files (***country_cords.tsv***, ***distanceMatrix.tsv***) into the database. Please make sure the table names are the same as those provided here.

```shell
# Start SQLite
sqlite3 database_name.sqlite

.mode tabs
.import distanceMatrix.tsv distM
.import country_cords.tsv country_cords
```

### 6.8. Edit global.R

If you have made it so far, congrats! You are one step away from being able to import your own data into TSST. Navigate to line 18 in the script [global.R](global.R), and edit the line to match the path/name of the new database file.

```R
# Original line
dbPath = "data/reich_v50.sqlite"

# New line
dbPath = "path/to/new/database"
```

TSST should now run normally with the new dataset. Please note **AGAIN** that this is not an officially supported feature yet and may not work properly.