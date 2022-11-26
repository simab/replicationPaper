library(raster)
library(rgdal)
################################################################################
## Combining the 34 "gtopo" raster tiles into a single, downsampled file
## Download URL: https://earthexplorer.usgs.gov/
## Download instructions: Requires registration at USGS. Manually download 34 
## individual files from the Earthexplorer tool (search for GTOPO30).
################################################################################

## Set working directory to own project folder
setwd("")
files <- list.files(pattern=".tif$")

## Remove antarctica file
files <- files[!grepl("antarc", files)]

## Load all raster files into list
gtopo.list <- lapply(1:length(files), function(x){
  raster(files[x])
})

## Stitch together into single raster
gtopo.full <- do.call(merge, gtopo.list)

## Downsample resolution
gtopo.full.004res <- aggregate(gtopo.full, fact = (0.04 / res(gtopo.full))[1])

## Export downsampled version 
writeRaster(gtopo.full.004res, 
            filename=paste0("gtopo_full_004res.tif"), 
            format="GTiff", overwrite=TRUE)