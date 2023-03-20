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

While in the future, we wish to support running TSST directly from this GitHub repository, due to the large filesize of the SQL database, it is not possible to do so at the moment. In order to run this application, you have to download the following files from this repo:

* **global.R**
* **server.R**
* **ui.R**
* **styles.css**
* data/**reich_v50.sqlite**

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

shiny::runApp("Absolute/Path/To/Root/Directory", launch.browser=FALSE)
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

The **Summary** tab next to the controls panel will automatically generate summary plots based on the options selected in the controls panel. There are no options to modify the graphical options of the plots generated in the current version, but will be added in future updates.

The **Data Explorer** can be used to easily navigate through and examine the raw data loaded into the application. Modifying the various options on the top panel will filter the data to be displayed on the data table below.

## 6. Inputting new data

Unfortunately, the current version of TSST does not officially support adding new data other than the dataset preloaded in the application. However, it is possible to generate your own SQLite database file using the scripts available in this repo.

**Disclaimer: This is not an officially supported function of TSST at the moment. Please not that the scripts may not function as intended when using your own data.**

