library(foreign)
library(tidyverse)
library(sf)
library(raster)
library(here)
library(stars)
library(terra)
library(lubridate)
here()

an.df <- read.dta(here("redemption-through-rebellion-dataverse_files","epr_segment_level_analysis.dta"))
geoepr.ag <- st_read(here("redemption-through-rebellion-dataverse_files","geoepr_ag.geojson"))

# must download ruggedness to fully replicate, large file
# from https://dataverse.harvard.edu/dataset.xhtml;jsessionid=b7ee5ce128efcce5938ff72864d2?persistentId=doi%3A10.7910%2FDVN%2FWXUZBN&version=&q=&fileAccess=&fileTag=&fileSortField=&fileSortOrder=
ruggedness_1k <- raster(here("terrain_ruggedness_dataverse_files","Ruggedness_OneKilometerData","ruggedness1K.tif"))
# follow process in `01_prepare_main_analysis_data.R` to replace NA with 0's (I think) reclassifying
ruggedness_1k <- reclassify(gtopo, cbind(NA, 0))
plot(ruggedness_1k)

rugged_vals <- raster::extract(ruggedness_1k,geoepr.ag, fun=mean, weights=TRUE)
rugged_vals_med <- raster::extract(ruggedness_1k,geoepr.ag, fun=median)
rugged_vals_max <- raster::extract(ruggedness_1k,geoepr.ag, fun=max)
rugged_vals_min <- raster::extract(ruggedness_1k,geoepr.ag, fun=min)
rugged_vals_range <- rugged_vals_max - rugged_vals_min



# Are the battles occurring inside vs outside AG territory?

geoepr.ag_rugged <- geoepr.ag %>%
                    mutate(rugged_mean = unlist(rugged_vals),
                           rugged_med = unlist(rugged_vals_med),
                           rugged_range = unlist(rugged_vals_range),
                           rugged_max = unlist(rugged_vals_max),
                           rugged_min = unlist(rugged_vals_min))

geoepr.ag_rugged_nosf <- geoepr.ag_rugged %>%
                          st_set_geometry(NULL) %>%
                          mutate(sdate = as.Date(sdate), edate=as.Date(edate)) %>%
                          mutate(syear = lubridate::year(sdate), eyear = lubridate::year(edate)) %>%
                          select(-c('dateiv','sdate','edate'))                         


an.df.rugged <- an.df %>%
                # some ags not in geoepr.ag dataframe, so don't join back into an.df
                # let these be NA
                merge(geoepr.ag_rugged_nosf, by='ag_id', all.x=TRUE) %>%
                filter((syear <= year & eyear >= year) | is.na(syear)) %>%
                rename(groupname = 'groupname.x') %>%
                dplyr::select(-c("groupname.y","syear","eyear"))

write.csv(an.df.rugged, here("redemption-through-rebellion-dataverse_files","epr_segment_level_analysis_rugged.csv"))
