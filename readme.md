# TempoSpatial SNP Tracker

## 1. Introduction

TempoSpatial SNP Tracker (TSST) is a tool aimed at simplifying the visualization of large SNP datasets with both temporal and geospatial components. TSST will produce an interactive map with a timeline and plots of their choice visualizing the loaded SNP data, allowing users to easily flip through the entire time range of their data. With an intuitive and modern UI, users can flexibily subset and cluster their data. Extensive graphical options allow users to fully customize the resulting map to their needs. TSST is also capable of producing summary plots displaying the global allele frequencies/counts for the chosen SNPs over time. Finally, the data explorer allows users to easily search and examine the raw data.

## 2. Web deployment

The application is available at the following [link](https://robinyn.shinyapps.io/tsst/).

**Note:** Due to the limitations of the free hosting service provided by Shinyapps.io, the performance of the web version is rather poor. Please allow the application to have plenty of time to render the map after changing any of the options. Downloading and running the application will result in better performance, but there may be compatibility issues.

## 3. Installation

These steps are ***not necessary*** if you decide to run the web version of the application available in the link above. Please only follow these instructions if you want to run the application on your local machine.

### 3.1. Required packages

The application is written in R (version 4.2.1) and the following packages are required:

* R Shiny (v. 1.7.4)
* Shinywidgets (v. 0.7.6)
* Tidyverse (v. 1.3.2)
* R Leaflet (v. 2.1.1)
* Leaflet.minicharts (v. 0.6.2)
* RSQLite (v. 2.3.0)
* RColorBrewer (v. 1.1_3)
* Plotly (v. 4.10.1)

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

#### **3.2.1. Running from GitHub**

R Shiny allows users to run applications hosted on GitHub directly without having to download any files. 