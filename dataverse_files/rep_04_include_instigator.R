library(xSub)
library(meltt)
library(dplyr)
library(here)


acd2epr <- read_csv(here('raw_epr_ucd_data','ACD2EPR-2021.csv'))
epr_ag_seg <- read_csv(here('raw_epr_ucd_data','EPR-AG_segment_level_dataset.csv'))
ged221 <- read_csv(here("ged221-csv","GEDEvent_v22_1.csv"))

ged221_filt <- ged221 %>%
  select(c("id","relid","year","type_of_violence","conflict_new_id","dyad_new_id",
           "side_a_new_id","side_b_new_id","country","where_coordinates","adm_1","adm_2",
           "latitude","longitude","geom_wkt","priogrid_gid","date_start","date_end"))

acd2epr_filt <- acd2epr %>%
  select(c("gwid","dyadid","statename","sidea_id","sidea","sideb_id",
           "sideb","group","gwgroupid","claim","recruitment","support"))

ged_acd2epr <- ged221_filt %>%
  merge(acd2epr_filt, by.x = c('dyad_new_id','side_a_new_id','side_b_new_id'), 
        by.y = c("dyadid","sidea_id","sideb_id"))
