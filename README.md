# HIecoH
Ecohydrology Analysis in Hawaii

Calculating probability of runoff across the landscape

# Data
“data/” folder contains required data for processing steps
All data in this folder has been created and published by other work

“infiltration_data/” contains original and processed infiltration (kfs) data
Final output manually created by combining all previously known kfs data
“kfs_data/hiecoh_kfs_data.csv”

# Code
1_prcp_pdfs_higap.R

Extracts and processes historical and future (pgw) data from NCAR NC files

Determines HIGAP landcover class for extracted data at 90 m 


2_higap_runoff_prob.R

Calculates runoff probabilities per HIGAP class for historical and future data


3_single_runoff_prob.R

Repeats Script #2 methods but does not separate landscape by landcover type 

Calculates runoff probabilities if entire landscape was a single HIGAP class

