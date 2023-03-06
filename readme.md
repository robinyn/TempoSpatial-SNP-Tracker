# Population Genetics Project - Tracing the allelic changes of SNPs over time and space

In this project, an R-shiny application that allows users to input plink files and the corresponding time dataset in order to visualize the allelic changes of selected SNPs over time and space on a map. 

## Data

Four data files were provided:
   
    - DataS1.bed
    - DataS1.bim
    - DataS1.fam
    - DataS1.xlsx (Eurasian - Dataset_tims.xlsx)

The name of the excel file "Eurasian-Dataset_tims" was changed to DataS1.xlsx for consistency with the other data files. 

### Data preprocessing

For the purposes of this project, the majority of the columns in the excel file was not needed. Therefore, the following columns were removed:

    - Ancient/Modern components
    - Age at death estimage
    - Converge on autosomal targets
    - SNPs hit on autosomal targets
    - Mean length of shotgun sequences
    - Family ID and position within family
    - Y/mtDNA data


