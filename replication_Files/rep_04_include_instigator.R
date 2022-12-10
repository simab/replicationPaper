library(xSub)
library(meltt)
library(dplyr)
library(here)
library(readr)
library(stringr)
library(foreign)



acd2epr <- read_csv(here('raw_epr_ucd_data','ACD2EPR-2021.csv'))
epr_ag_seg <- read_csv(here('raw_epr_ucd_data','EPR-AG_segment_level_dataset.csv'))

# too large to include in repository;
# to replicate, download 'UCDP Georeferenced Event Dataset (GED) Global version 22.1'
# from https://ucdp.uu.se/downloads/index.html#ged_global
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
        by.y = c("dyadid","sidea_id","sideb_id")) %>%
  filter(!is.na(gwgroupid),
         type_of_violence==1,
         claim >= 1,
         recruitment == 1)

# id number of rows of AG-year-country conflict
# to compare with ultimate joined COW data
ged_acd2epr_pre2006 <- ged_acd2epr_agg_noadm %>%
              filter((year <= 2006) & 
                      (year >= 1989))


ged_acd2epr_agg <- ged_acd2epr %>%
                  dplyr::select(-c(id, relid, where_coordinates, latitude,
                                   longitude, geom_wkt, priogrid_gid, date_start,
                                   date_end)) %>%
                  mutate(sidea_nogov = str_replace(sidea, "Government of ","")) %>%
                  unique

ged_acd2epr_agg_noadm <- ged_acd2epr %>%
                          dplyr::select(-c(id, relid, where_coordinates, latitude,
                                           longitude, geom_wkt, priogrid_gid, date_start,
                                           date_end,adm_1,adm_2)) %>%
                          mutate(sidea_nogov = str_replace(sidea, "Government of ","")) %>%
                          unique


# id number of rows of AG-year-country conflict
# to compare with ultimate joined COW data
ged_acd2epr_pre2006_nrow <- ged_acd2epr_agg_noadm %>%
  filter((year <= 2006) & 
           (year >= 1989)) %>%
  nrow()


ged_acd2epr_dyads <- ged_acd2epr %>%
                    dplyr::select(c(dyad_new_id, side_a_new_id, side_b_new_id,
                                    country, statename, sidea, sideb, group, gwgroupid)) %>%
                  mutate(sidea_nogov = str_replace(sidea, "Government of ","")) %>%
                  unique()



#------------------------- CORRELATES OF WAR -------------------------#
#---------------------------------------------------------------------#

# Non-state wars include NO state entities, so not right for our purposes
# https://correlatesofwar.org/wp-content/uploads/Non-StateWars_Codebook.pdf

# Intra-state wars within state territory, 4 types:
# civil wars (for central control (4) and local issues (5)) - both included
# regional internal (govt of regional subunit against nonstate entity (6)) - included
# intercommunal (combat b/w 2+ nonstate entities (7)) - NOT included

# Extra-state wars (between state and nonstate-entity outside state borders), 2 types:
# Colonial wars (adversary is colony/dependency/protectorate) (2) - Included
# Imperial wars (state fights non-state, independent political entity) (3) - Included


#---------------------- DATA INPUT + PREP -----------------------------#
cow_intrastate <- read.csv(here("cow_data","Intra-StateWarData_v4.1.csv"))
cow_intra_df <- cow_intrastate %>%
                filter(WarType %in%  c(4, 5, 6),
                       StartYear1 >= 1946) %>%
                dplyr::select(-c("TransTo","SideADeaths","SideBDeaths",
                                 "Version","Intnl"))


cow_extrastate <- read.csv(here("cow_data","Extra-StateWarData_v4.0.csv"))
cow_extra_df <- cow_extrastate %>%
                      filter(StartYear1 >= 1946) %>%
                      dplyr::select(-c("TransTo","BatDeath",
                                       "NonStateDeaths","Version","Interven")) %>%
                      rename("CcodeA"="ccode1", "CcodeB"="ccode2")

cow_dfs <- rbind(cow_intra_df, cow_extra_df) %>%
                  # remove conflicts where both sides unknown 
                  # especially removes additional rows for conflicts where outside
                  # power interferes and is given their own row (eg, WarNum 766)
                  filter(SideA != -8, SideB != -8) %>%
                  # assuming from data that Initiator == 1 also means State initiated 
                  mutate(initiator_state = ifelse((Initiator == SideA)|(Initiator == 1), 1, 0)) %>%
                  # one conflict where state instigated but different text used
                  mutate(initiator_state = ifelse(WarNum == 932, 1, initiator_state))
                  

#------------------- JOINING COW + ACD -----------------------------#

# country is sometimes not the same as sidea in ace_dyads
acd_dyads_cows_rough <- merge(cow_dfs, ged_acd2epr_dyads, by.x="SideA",
                              by.y = "sidea_nogov", all = TRUE)
write.csv(acd_dyads_cows_rough, "acd_dyads_cow_matching.csv")


acd_dyads_cows_years_rough <- merge(cow_dfs, ged_acd2epr_agg_noadm,
                                    by.x = "SideA", by.y="sidea_nogov",
                                    all=TRUE) %>%
                              filter(((year >= StartYear1) & (year <= EndYear1)) |
                                    ((year >= StartYear2) & (year <= EndYear2))) %>%
                              select(-c("StartDay1","StartMonth1","EndDay1","EndMonth1",
                                        "StartDay2","StartMonth2","EndDay2","EndMonth2",
                                        "TransFrom","WhereFought"))
acd_dyads_cows_years_rough_gb <- acd_dyads_cows_years_rough %>%
                                group_by_at(vars(-year)) %>%
                                summarize(min_year = min(year),
                                          max_year = max(year)) %>%
                                ungroup()

write.csv(acd_dyads_cows_years_rough_gb, "acd_dyads_cows_years_rough.csv")


#-------------------------- INCORPORATE MATCHED DATA ---------------------#

acd_cows_years_matched <- read.csv("acd_dyads_cows_years_rough_matched.csv")

# keep only certain matches
acd_cows_matched_1 <- acd_cows_years_matched %>%
                      filter(Confirmed == 1) %>%
                      select(-c("Notes"))

acd_cows_matched_1_onlycow <- acd_cows_matched_1 %>%
                              select(-c("X","SideA","WarNum","WarName","WarType","CcodeA","CcodeB",          
                                        "SideB","StartYear1","EndYear1", "StartYear2","EndYear2", "Initiator","Outcome"))

# keep certain and maybe matches
acd_cows_matched_01 <- acd_cows_years_matched %>%
                       filter(Confirmed >= 0) %>%
                       select(-c("Notes"))

acd_cows_matched_01_onlycow <- acd_cows_matched_01 %>%
  select(-c("X","SideA","WarNum","WarName","WarType","CcodeA","CcodeB",          
            "SideB","StartYear1","EndYear1", "StartYear2","EndYear2", "Initiator","Outcome"))



# join back to GED, filter for only confirmed
# don't join to data with full lat/long, since can't confirm which goes where
ged_cow <- merge(ged_acd2epr_agg_noadm, acd_cows_matched_1_onlycow,
                 by = c("dyad_new_id", "side_a_new_id", "side_b_new_id", 
                          "conflict_new_id", "country", "gwid", "sidea", "sideb", 
                          "gwgroupid", "claim", "recruitment", "support",
                        "statename","group","type_of_violence")) %>%
            filter(year >= min_year & year <= max_year) 


write.csv(ged_cow, "ged_cow_confirmed1.csv")

# join confirmed + maybe back to GED
ged_cow01 <- merge(ged_acd2epr_agg_noadm, acd_cows_matched_01_onlycow,
                 by = c("dyad_new_id", "side_a_new_id", "side_b_new_id", 
                        "conflict_new_id", "country", "gwid", "sidea", "sideb", 
                        "gwgroupid", "claim", "recruitment", "support",
                        "statename","group","type_of_violence")) %>%
  filter(year >= min_year & year <= max_year)

write.csv(ged_cow01, "ged_cow_confirmedmaybe01.csv")


#------------------------------ ADD TFRAC -----------------------------------#

an.df <- read.dta(here("redemption-through-rebellion-dataverse_files","epr_segment_level_analysis.dta"))

an.df$pys <- an.df$peaceyears
an.df$pys2 <- an.df$peaceyears^2
an.df$pys3 <- an.df$peaceyears^3

tfrac_group <- an.df %>%
              select(c(year, gwgroupid, countries_gwid, ag_id, split, tfrac, tfrac_incr, tfrac_incr_post1946,
                       ln_ag_area_sqkm, status_excl, downgraded2, rbal, warhist, 
                       ln_capdist, ln_rgdppc_lag, ln_pop_lag, colonial_past, ln_state_age, ag_incidence_flag_lag,
                       pys, pys2, pys3)) %>%
              unique()
ged_cow_tojoin <- ged_cow %>%
  # remove country field from epr data - potential for using it to identify extrastate? 
  # but secondary concern
  select(-c(min_year, max_year, country)) %>%
  unique()

# we lose another 44 observations joining with tfrac.. 
ged_cow_tfrac <- merge(ged_cow_tojoin, tfrac_group, by.x = c("year", "gwid", "gwgroupid"),
                       by.y = c("year", "countries_gwid", "gwgroupid"))

ged_cow_tfrac_lm <- lm(initiator_state ~ tfrac, data=ged_cow_tfrac)
summary(ged_cow_tfrac_lm)

# We can include all of the controls present in the original regression run by Cederman et al., as they are all year-specific to the particular AG and country in which the conflict occurs.
# Should we include peace years, since our COW data is different? Some are not included in this? 


