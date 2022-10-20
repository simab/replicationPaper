################################################################################
## Redemption Through Rebellion: Border Change, Lost Unity and Nationalist Conflict
## Cederman, Rüegger and Schvitz, 2020
## Replication files
## Contact:  Guy Schvitz, guy.schvitz@gess.ethz.ch
###############################################################################

###############################################################################
## Overview
###############################################################################

This folder contains all the data and code to replicate the tables 
and figures shown in the main paper and the online appendix. 

## Operating system
Data preparation and analysis were done using R version 4.0.2 (2020-06-22) and 
Stata version 15.0 on a Macbook pro (OSX Mojave, 10.14.5). 
Platform: x86_64-apple-darwin17.0

## R packages (run script "00_install_packages.R" to install them)

- From CRAN: "tidyverse", "sf", "nngeo", "cshapes", "raster", "lubridate", 
"foreign", "parallel", "devtools", "texreg", "brglm", "mediation", "haven", 
"vdem", "lfe", "rgdal"

- From the CRAN Archive: "velox"

- From Github: "gsfuns"

## Stata packages (run script "00_install_packages_stata.do" to install them)
acreg, estout, marginscontplot

## Estimated runtime for all data preparation and analysis:
0.5-2 hours

###############################################################################
## Analysis datasets
###############################################################################

The analyses use the following datasets:

- epr_segment_level_analysis.dta - main models and mediation models

- eur_map_1918_analysis.dta - models using the ethnographic map from 1918

- murdock_anaylsis - models using Murdock's tribal map of Africa

- epr_ag_level_analysis.dta - used in robustness (appendix)

The file "Codebook.pdf" gives the definitions and sources of all variables used in the analysis

###############################################################################
## Source datasets
###############################################################################

The analysis datasets are created from the following source datasets:

- epr_ag_level_dataset.csv - Schvitz, Guy (2018). Data on Aggregate Ethnic Groups (AG-Level Dataset), 1946-2017. https://icr.ethz.ch/data/epr/ag/

- epr_segment_level_dataset.csv - Schvitz, Guy (2018). Data on Aggregate Ethnic Groups (Segment-Level Dataset), 1946-2017. https://icr.ethz.ch/data/epr/ag/

- geoepr_ag.geojson - Schvitz, Guy (2018). Data on Aggregate Ethnic Groups (Aggregate Group Polygons), 1946-2017. https://icr.ethz.ch/data/epr/ag/

- epr_sdm_claims.csv - Schaedel, Andreas (2018). Self-Determination Claims by EPR groups, 1946-2017. Handcoded Dataset (Unpublished)

- epr_sdm_egip_claims.csv - Rueegger, Seraina (2018). Self-Determination Claims by EPR Groups in Power on Behalf of Co-Ethnics Abroad. Handcoded Dataset (Unpublished)

- cshapes_2.geojson - Schvitz, Guy, et al. (2020). The CShapes 2.0 Dataset. https://icr.ethz.ch/data/cshapes/

- eur_map_1918_polygons.geojson - Schvitz, Guy (2019). Ethnic Group Polygons Based on Historical Hap by J. Gabrys (1918). Handcoded Dataset (Unpublished)

- eur_map_1918_boudary.geojson - Schvitz, Guy (2019). Eastern Boundary of Historical Hap by J. Gabrys (1918). Handcoded Dataset (Unpublished)

- geoepr2eur_map.csv - Schvitz, Guy (2019). List of GeoEPR groups Linked to Ethnic Groups on Historical Map by J. Gabrys (1918). Handcoded Dataset (Unpublished)

- borders_tribes.geojson - Nunn, Nathan and Wantchekon, Leonard (2011). Murdock Ethnic Groups Polygons. https://scholar.harvard.edu/files/nunn/files/murdock_shapefile.zip

- popc_1880AD.asc - Klein Goldewijk, Kees et al (2011). HYDE Gridded Population Estimates of 1880. https://dataportaal.pbl.nl/downloads/HYDE/HYDE3.1/hyde31_final.zip

- gtopo_lowres.tif - U.S. Geological Service. (1996). Digital Elevation - Global 30 Arc-Second Elevation (GTOPO30). https://earthexplorer.usgs.gov/

Download instructions: Requires registration at USGS. Manually download 34 individual files from the Earthexplorer tool (search for GTOPO30). Stitch files together and downsample resolution using supplementary R-Script "not_run_gtopo30_prep.R"

- ged191.csv - Sundberg, Ralph and Melander, Erik (2013). UCDP Georeferenced Event Dataset (GED) Global version 19.1. https://ucdp.uu.se/downloads/olddw.html

All unpublished datasets were created by the authors of the paper or by other members of the ICR research group at ETH Zürich. All published datasets were made freely available by each dataset's authors for research purposes.

###############################################################################
## R-Code
###############################################################################

All data preparation and some of the analyses were done in R. To replicate the results, run these files in the following order:

- 00_install_packages.R - installs the necessary packages in R, if not already installed (NOTE: Must be used on a Mac in order to run correctly)

- 00_analysis_functions.R - helper functions used in the analyses

- 01_prepare_main_analysis_data	- prepares all the analysis datasets. this may take a while, depending on your computer

- 02_mediation_models.R - replicates the mediation models and plots shown in the paper and the appendix

- 03_robustness_appendix.R - replicates the robustness models shown in the appendix that were estimated in R

- not_run_gtopo30_prep.R - stitches together individual GTOPO30 raster files and downsamples resolution (not needed to replicate results) 

###############################################################################
## Stata do-files
###############################################################################

Most of the analyses shown in the main paper and appendix were done in stata. To replicate the results, run these files in the following order:

- 00_install_packages_stata.do - installs the necessary packages in stata

- 01_main_models_stata.do - replicates the models shown in the main text

- 02_additional_models_appendix_stata.do - replicates the additional models discussed in the main text and shown in the appendix

- 03_robustness_appendix_stata.do - replicates the robustness models shown in the appendix.

- 00_label_vars_stata - this file is called from the other script files, it assigns labels to each variable to be shown in the table output.

###############################################################################
## Setting your working directory
###############################################################################

Before running the files, set the first line in each stata do-file and R-script-file to your own working directory (the folder that contains this readme file, along with all the other files)

###############################################################################
## Other files
###############################################################################

- epr_sdm_claims_codebook.pdf: Provides information on how self-determination claims were coded for EPR groups

- epr_sdm_claims_documentation.pdf: Documents coding of self-determination claims for EPR groups on a case-by-case basis

- epr_sdm_egip_claims_coding_instructions.pdf: Provides information on how self-determination claims on behalf of kin groups abroad were coded for EPR groups

- epr_sdm_egip_claims_documentation.pdf: Documents coding of self-determination claims on behalf of kin groups abroad on a case-by-case basis
