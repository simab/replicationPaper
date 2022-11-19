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

# they do include elevation as a control, so we only need to include ruggedness
ruggedness_1k <- raster(here("terrain_ruggedness_dataverse_files","Ruggedness_OneKilometerData","ruggedness1K.tif"))
# follow process in `01_prepare_main_analysis_data.R` to replace NA with 0's (I think) reclassifying
ruggedness_1k <- reclassify(gtopo, cbind(NA, 0))
plot(ruggedness_1k)

rugged_vals <- raster::extract(ruggedness_1k,geoepr.ag, fun=mean, weights=TRUE)


geoepr.ag_rugged <- geoepr.ag %>%
                    mutate(rugged_mean = unlist(rugged_vals))

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
