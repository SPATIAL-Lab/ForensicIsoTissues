# ForensicIsoTissues
Data and scripts for forensic body tissue isotope compilation and data review titled "Human Tissue Oxygen and Strontium Isotope Values in North America: A Data Compilation and Assessment for Forensic Geolocation."

## data/
Data used in analyses.

- **FIT_Dataset.xlsx** Compiled Dataset of &delta;<sup>18</sup>O data and <sup>87</sup>Sr/<sup>86</sup>Sr isotopic data from human tissues in North America.
- **RefIDsRefNo.xlsx** Spreadsheet to used to integrate references citations numbers with Reference.IDs in the complied dataset. 

## code/
Scripts used for data analysis and plotting.

- **FITDataSetup.R** Load data and set up for analysis. Includes some descriptive statistics.
- **FITMapping.R** Create maps of spatial distribution of data for hair and tooth enamel isotopic values.
- **FITTapiso.R** Analyze data using isoscapes and statistics. Includes code for generation of figures. 

## shapefiles/
Includes all shapefiles and geospatial data required for running analyses that are not pulled from other R packages. 

## figures/
Out bound folder for figures and figure components generated in the script.
