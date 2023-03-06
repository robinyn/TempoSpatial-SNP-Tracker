# Population Genetics Project - Tracing the allelic changes of SNPs over time and space

In this project, an R-shiny application that allows users to input plink files and the corresponding time dataset in order to visualize the allelic changes of selected SNPs over time and space on a map. 

## 1. Development environment

The majority of the development for this app was done on an M1 mac but certain parts of the analysis were performed on the course server (GNU Linux x86_64). Softwares and analysis run on the server are denoted by an asterix in the rest of the readme file. The following softwares were used:

    - Plink (version 1.90b7 64-bit)*
    - R 

Plink was downloaded from the official [website](https://www.cog-genomics.org/plink/1.9/) and added to the PATH. 
## 2. Data

Four data files were provided:
   
    - DataS1.bed
    - DataS1.bim
    - DataS1.fam
    - DataS1.xlsx (Eurasian - Dataset_tims.xlsx)

The name of the excel file "Eurasian-Dataset_tims" was changed to DataS1.xlsx for consistency with the other data files. 

### 2.1. Data preprocessing

#### 2.1.1. Excel

For the purposes of this project, the majority of the columns in the excel file was not needed. Therefore, the following columns were removed:

    - Ancient/Modern components
    - Age at death estimage
    - Converge on autosomal targets
    - SNPs hit on autosomal targets
    - Mean length of shotgun sequences
    - Family ID and position within family
    - Y/mtDNA data

#### 2.1.2. Plink*

Due to compatibility issues with the Plink software, this part of the preprocessing had to be conducted on the course server. In order to extract the SNP data from the three plink files (DataS1.bed/.bim/.fam), the files were uploaded to the server and the following commands were used in the same directory as the data files:

```shell
plink --bfile DataS1 --recode compound-genotype --out DataS1
```

The resulting uncompressed plink files were downloaded to the local machine.  
