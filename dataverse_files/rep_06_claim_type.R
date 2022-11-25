library(here)
library(haven)
library(tidyverse)

#datasets from Samabis 2020
greivance_escal_df_sambanis = read_dta(here("sambanis_paper_rep_materials", 
                                            "GS_grievances_escalation.dta")) %>% #some odd joining in the og dataset
  select(gwgroupid, year, statusname, nviolsd_violsd, 
         lost_autonomy, downgr5_aut, downgr5_incl)  

combine_dfs = epr.claims %>%
  filter(year <= 2012) %>%
  left_join(greivance_escal_df_sambanis, by = c("gwgroupid", "year"))

View(epr.claims %>%
       filter(year <= 2012) %>%
  anti_join(greivance_escal_df_sambanis, by = c("gwgroupid", "year")))

dim(epr.claims)
dim(greivance_escal_df_sambanis)
dim(combine_dfs)
  
  
  greivance_escal_df_sambanis %>%
  select(gwgroupid, year, statusname, nviolsd_violsd, 
         lost_autonomy, downgr5_aut, downgr5_incl) 



  
  full_join(lost_autonomy_df_sambanis, by = c("gwgroupid")) %>%
  select((contains(".y")))


test = epr.claims %>%
  full_join(greivance_escal_df_sambanis)
