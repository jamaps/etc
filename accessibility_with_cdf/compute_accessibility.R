library(tidyverse)
library(reshape2)

setwd("~/Dropbox/work/OntarioLine")


# load in the accessibility f(tij) from the cdf

cdf <- read.csv("spatial_data/cdf_traveltimes.csv")
cdf$cdf <- NULL

# load in destination data:

Oj <- read.csv("spatial_data/tabular_data/taz_with_2016_emp_data.csv")
# remove unneeded columns
Oj$X <- NULL

Oj_pop <- read.csv("spatial_data/tabular_data/census_taz_gtha.csv")
Oj_pop <- Oj_pop[,c("taz","pop","dwellings")]

Oj <- merge(Oj_pop, Oj, by = "taz", all.x = T)

rownames(Oj) <- paste("X", Oj$taz, sep="")
Oj$taz <- NULL

Oj[is.na(Oj)] <- 0


# bring in the travel times!

# load
times_nc <- read.csv("fromMX/DATA FOR UofT/perceived_tt_BAU.csv") # , row.names = 1 
times_nc <- subset(times_nc,times_nc$X != "e+20") # remove the weird rows

# add in row names
rownames(times_nc) <- paste("X",times_nc$X,sep= "")
times_nc$X <- NULL

# add 5 min travel time to internal zone trips
times_nc <- times_nc + diag(3530) * 5

# round to save space
times_nc <- round(times_nc,digits = 0)

times_nc[times_nc > 300] <- 300

# just gtha
times_nc <- times_nc[0:2265,0:2265]



# load
times_ol <- read.csv("fromMX/DATA FOR UofT/perceived_tt_OL.csv") # , row.names = 1 
times_ol <- subset(times_ol,times_ol$X != "e+20") # remove the weird rows

# add in row names
rownames(times_ol) <- paste("X",times_ol$X,sep= "")
times_ol$X <- NULL

# add 5 min travel time to internal zone trips
times_ol <- times_ol + diag(3530) * 5

# round to save space
times_ol <- round(times_ol,digits = 0)

times_ol[times_ol > 300] <- 300

# just gtha
times_ol <- times_ol[0:2265,0:2265]



# # # 

# compute access for No Change (NC) business as usual

# to long table format
dfa <- melt(as.matrix(times_nc))
colnames(dfa) <- c("O","D","tt")

# join in CDF data
dfa <- merge(dfa,cdf,by.x = "tt", by.y = "travel_time")

# join Oj data
dfa <- merge(dfa, Oj, by.x = "D", by.y = 0)

dfataz <- dfa %>% group_by(O) %>%
  summarise(
    pop = sum(pop * one_minus_cdf),
    dwellings = sum(dwellings * one_minus_cdf),
    total_emp = sum(total_emp * one_minus_cdf),
    NOC_total = sum(NOC_total * one_minus_cdf),
    NOC_0_mgmt = sum(NOC_0_mgmt * one_minus_cdf),
    NOC_1_business = sum(NOC_1_business * one_minus_cdf),
    NOC_2_sciences = sum(NOC_2_sciences * one_minus_cdf),
    NOC_3_health = sum(NOC_3_health * one_minus_cdf),
    NOC_4_edu_law_gov_com = sum(NOC_4_edu_law_gov_com * one_minus_cdf),
    NOC_5_art_cult_rec_sport = sum(NOC_5_art_cult_rec_sport * one_minus_cdf),
    NOC_6_sales_service = sum(NOC_6_sales_service * one_minus_cdf),
    NOC_7_trades_transp_equip = sum(NOC_7_trades_transp_equip * one_minus_cdf),
    NOC_8_agri_nat_resource = sum(NOC_8_agri_nat_resource * one_minus_cdf),
    NOC_9_manf_util = sum(NOC_9_manf_util * one_minus_cdf),
    NAICS_total = sum(NAICS_total * one_minus_cdf),
    NAICS_11_agri_fores_fishing = sum(NAICS_11_agri_fores_fishing * one_minus_cdf),
    NAICS_22_utilities = sum(NAICS_22_utilities * one_minus_cdf),
    NAICS_23_construction = sum(NAICS_23_construction * one_minus_cdf),
    NAICS_31_32_33_manufacturing = sum(NAICS_31_32_33_manufacturing * one_minus_cdf),
    NAICS_41_wholesale_trade = sum(NAICS_41_wholesale_trade * one_minus_cdf),
    NAICS_44_45_retail_trade = sum(NAICS_44_45_retail_trade * one_minus_cdf),
    NAICS_48_89_transport_warehouse = sum(NAICS_48_89_transport_warehouse * one_minus_cdf),
    NAICS_51_info_cult = sum(NAICS_51_info_cult * one_minus_cdf),
    NAICS_52_finance_insurance = sum(NAICS_52_finance_insurance * one_minus_cdf),
    NAICS_53_real_estate = sum(NAICS_53_real_estate * one_minus_cdf),
    NAICS_54_scient_tech_professional = sum(NAICS_54_scient_tech_professional * one_minus_cdf),
    NAICS_55_mgmt_of_companies = sum(NAICS_55_mgmt_of_companies * one_minus_cdf),
    NAICS_56_admin_wastemngt_remediation = sum(NAICS_56_admin_wastemngt_remediation * one_minus_cdf),
    NAICS_61_edu = sum(NAICS_61_edu * one_minus_cdf),
    NAICS_62_health_social_assist = sum(NAICS_62_health_social_assist * one_minus_cdf),
    NAICS_71_arts_ent_rec = sum(NAICS_71_arts_ent_rec * one_minus_cdf),
    NAICS_72_accomidatoin_food_services = sum(NAICS_72_accomidatoin_food_services * one_minus_cdf),
    NAICS_81_other = sum(NAICS_81_other * one_minus_cdf),
    NAICS_91_public_admin = sum(NAICS_91_public_admin * one_minus_cdf)
  )

dfataz$O <- substring(dfataz$O, 2)

write.csv(dfataz,"taz_accessibility_2016_NC.csv")


# same for ontario line

# compute access for No Change (NC) business as usual

# to long table format
dfa <- melt(as.matrix(times_ol))
colnames(dfa) <- c("O","D","tt")

# join in CDF data
dfa <- merge(dfa,cdf,by.x = "tt", by.y = "travel_time")

# join Oj data
dfa <- merge(dfa, Oj, by.x = "D", by.y = 0)

dfataz <- dfa %>% group_by(O) %>%
  summarise(
    pop = sum(pop * one_minus_cdf),
    dwellings = sum(dwellings * one_minus_cdf),
    total_emp = sum(total_emp * one_minus_cdf),
    NOC_total = sum(NOC_total * one_minus_cdf),
    NOC_0_mgmt = sum(NOC_0_mgmt * one_minus_cdf),
    NOC_1_business = sum(NOC_1_business * one_minus_cdf),
    NOC_2_sciences = sum(NOC_2_sciences * one_minus_cdf),
    NOC_3_health = sum(NOC_3_health * one_minus_cdf),
    NOC_4_edu_law_gov_com = sum(NOC_4_edu_law_gov_com * one_minus_cdf),
    NOC_5_art_cult_rec_sport = sum(NOC_5_art_cult_rec_sport * one_minus_cdf),
    NOC_6_sales_service = sum(NOC_6_sales_service * one_minus_cdf),
    NOC_7_trades_transp_equip = sum(NOC_7_trades_transp_equip * one_minus_cdf),
    NOC_8_agri_nat_resource = sum(NOC_8_agri_nat_resource * one_minus_cdf),
    NOC_9_manf_util = sum(NOC_9_manf_util * one_minus_cdf),
    NAICS_total = sum(NAICS_total * one_minus_cdf),
    NAICS_11_agri_fores_fishing = sum(NAICS_11_agri_fores_fishing * one_minus_cdf),
    NAICS_22_utilities = sum(NAICS_22_utilities * one_minus_cdf),
    NAICS_23_construction = sum(NAICS_23_construction * one_minus_cdf),
    NAICS_31_32_33_manufacturing = sum(NAICS_31_32_33_manufacturing * one_minus_cdf),
    NAICS_41_wholesale_trade = sum(NAICS_41_wholesale_trade * one_minus_cdf),
    NAICS_44_45_retail_trade = sum(NAICS_44_45_retail_trade * one_minus_cdf),
    NAICS_48_89_transport_warehouse = sum(NAICS_48_89_transport_warehouse * one_minus_cdf),
    NAICS_51_info_cult = sum(NAICS_51_info_cult * one_minus_cdf),
    NAICS_52_finance_insurance = sum(NAICS_52_finance_insurance * one_minus_cdf),
    NAICS_53_real_estate = sum(NAICS_53_real_estate * one_minus_cdf),
    NAICS_54_scient_tech_professional = sum(NAICS_54_scient_tech_professional * one_minus_cdf),
    NAICS_55_mgmt_of_companies = sum(NAICS_55_mgmt_of_companies * one_minus_cdf),
    NAICS_56_admin_wastemngt_remediation = sum(NAICS_56_admin_wastemngt_remediation * one_minus_cdf),
    NAICS_61_edu = sum(NAICS_61_edu * one_minus_cdf),
    NAICS_62_health_social_assist = sum(NAICS_62_health_social_assist * one_minus_cdf),
    NAICS_71_arts_ent_rec = sum(NAICS_71_arts_ent_rec * one_minus_cdf),
    NAICS_72_accomidatoin_food_services = sum(NAICS_72_accomidatoin_food_services * one_minus_cdf),
    NAICS_81_other = sum(NAICS_81_other * one_minus_cdf),
    NAICS_91_public_admin = sum(NAICS_91_public_admin * one_minus_cdf)
  )

dfataz$O <- substring(dfataz$O, 2)

write.csv(dfataz,"taz_accessibility_2016_OL.csv")
