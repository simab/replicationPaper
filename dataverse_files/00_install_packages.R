################################################################################
## This script installs all the R packages required to run the other R scripts 
## in this folder, if they are not already installed.
################################################################################

## Clear workspace
rm(list = ls())


## Define required packages
pkgs <- c("tidyverse", "sf", "nngeo", "cshapes", "raster", "lubridate", "foreign",
          "parallel", "devtools", "texreg", "brglm", "mediation", "haven",
          "lfe", "rgdal", "rgeos", "lwgeom")

## Check if packages are installed, install if not
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.pkgs)>0) install.packages(new.pkgs)
invisible(lapply(pkgs, library, character.only =T))
rm(pkgs, new.pkgs)

## Install packages not on CRAN
#devtools::install_version("velox", version = "0.2.0",
 #                         repos = "http://cran.us.r-project.org")
devtools::install_github("hunzikp/velox@master")
devtools::install_github("xmarquez/vdem")
devtools::install_github("guyschvitz/gsfuns")
