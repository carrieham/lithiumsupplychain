install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, openxlsx, sf)

rm(list = ls())

#load data
oxfam <- read_csv("state/oxfam.csv")
bea <- read_csv("state/bea_gdp.csv")
wage <- read_csv("state/dol_minimumWage.csv") %>%
  mutate(Minimum_Wage_map=as.numeric(substring(Minimum_Wage,2))) %>%
  mutate(Minimum_Tipped_Wage_map=as.numeric(substring(Minimum_Tipped_Wage,2)))
state_income <- st_read("state/ACS_Median_Household_Income_Variables/State.shp") %>%
  select(GEOID,NAME,B19049_001,geometry)%>%
  rename("Median_Income"="B19049_001",
         "State_or_Territory"="NAME")

#clean oxfam data
names(oxfam) <-str_replace_all(names(oxfam)," ", "_")
oxfam <- oxfam %>%
  mutate(across(c(Local_Control_of_Minimum_Wage,
                  Private_Sector_Pregnant_Worker_Accommodation:Sexual_Harassment_in_the_Law,
                  Domestic_Worker_Protections,
                  Heat_Standard,
                  Teachers_Collective_Bargaining:PLAs_Allowed_Locally),as.logical),
         across(where(is.logical), 
                ~ifelse(., "Yes", "No")),
         Farmworkers_Covered_by_Minimum_Wage=recode(as.factor(Farmworkers_Covered_by_Minimum_Wage),
                               "0"="No",
                               "0.5"="Yes, with some exceptions.",
                               "1"="Yes"),
         Farmworker_Compensation=recode(as.factor(Farmworker_Compensation),
                                        "0"="No",
                                        "0.5"="Partially covered or covered with exceptions.",
                                        "1"="Yes"),
         Protections_From_Retaliation_text=recode(as.factor(Protections_From_Retaliation),
                     "0"="No. Criminal penalities only against wage theft retaliation (no civil penalties).",
                     "0.5"="Yes. Policy includes back pay, recovered attorney fees, court cases against employers, and court-mandated monetary damages. Does not include government-imposed fine. No easy means to take complaint to government agency.",
                     "0.75"="Yes. Policy includes back pay, monetary damages, recovered attorney fees, and right to take complaints to a government agency and court. Does not include government-imposed fine.",
                     "1"="Yes. Policy includes back pay, monetary damages, recovered attorney fees, right to take complaints to a government agency and court, and potential for government-imposed fine."),
         Collective_Bargaining=recode(as.factor(Collective_Bargaining),
                             "0"="No",
                             "0.5"="Allowed",
                             "1"="Required")) 

#merge oxfam, gdp, and minimum wage data
state <- full_join(state_income,
                   oxfam,
                   by="State_or_Territory") %>%
  full_join(bea,by="State_or_Territory")%>%
  left_join(wage,by="State_or_Territory") %>%
  mutate(Wage_Ratio=round(Wage_Ratio*100,1)) 

#output
st_write(state,
         "state_final.geojson",
         append=FALSE)

st_write(state,
         "state_final.csv",
         append=FALSE)

