library(here)
library(haven)
library(tidyverse)

#load dataset from Germann and Sambanis 2020
grievance.escal.df = read_dta(here("sambanis_paper_rep_materials", 
                                            "GS_grievances_escalation.dta")) %>% 
  select(gwgroupid, year, statusname, nviolsd_violsd, 
         lost_autonomy, downgr5_aut, downgr5_incl)  

prop.type.action = grievance.escal.df %>%
  group_by(gwgroupid) %>%
  summarise(mean_action = mean(nviolsd_violsd)) %>%
  drop_na()

#load ruggedness analysis df
analysis.df = read.csv(here("redemption-through-rebellion-dataverse_files","epr_segment_level_analysis_rugged.csv"))

#merge grievance.escal.df with main analysis dataframe
exten.analysis.df = analysis.df %>%
  left_join(grievance.escal.df, by = c("gwgroupid", "year")) %>%
  left_join(prop.type.action)

#write extension analysis csv
write.csv(exten.analysis.df, 
          here("redemption-through-rebellion-dataverse_files",
               "epr_segment_level_analysis_extensions_rugged_claims.csv"))
