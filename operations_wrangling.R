install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, openxlsx, purrr)

rm(list = ls())

# cleaning raw dataset-------------------------------------------------------
#read naatbatt xlsx file
path <- "operations/operations_naatbatt_Dec2021.xlsx"
nodes <- c("1-RawMatl",                  
           "2-Battery Grade Materials", 
           "3-Other Battery Comps Matls",
           "4-Electrodes and Cells",     
           "5-ModPack",                  
           "6-EOL",                    
           "7-Equipment",              
           "8-ServiceRepair",          
           "9-RandD",                 
           "10-Modeling",              
           "11-Distributors")

operations <- nodes %>% 
  set_names() %>%
  map_df(read_excel, path = path , col_types="text", trim_ws=TRUE) %>%
  select(c("ID",
           "Node",
           "QC Date",
           "Team Member",
           "Status",
           "Status2",
           "Timeline",
           "Company",
           "Facility Name",
           "Facility Type",
           "Product Type",
           "Product",
           "Facility Workforce",
           "Production Capacity",
           "Facility or Company Website",
           "Facility Phone",
           "Facility City",
           "Facility State or Province",
           "Facility Country",
           "Data Source",
           "Latitude",
           "Longitude")) %>%
  rename("Supply Chain Segment"="Node",
         "Website"="Facility or Company Website",
         "Updated"="QC Date",
         "Workforce"="Facility Workforce") %>%
  mutate(Status=recode(Status,
                       "C"="Active","c"="Active",
                       "PC-SU"="Active",
                       "UC"="Under construction",
                       "P"="Planned","p"="Planned"),
         Status2=recode(Status,
                        "Under construction"="Planned or under construction",
                        "Planned"="Planned or under construction"),
         Updated=convertToDate(Updated),
         Workforce_dummy=replace_na(Workforce,0),
         Workforce=replace_na(Workforce,"Unknown"))

#merge with EV data
operations_ev <- read_csv("operations/operations_EV_Aug2022.csv")
operations_final <- bind_rows(operations,operations_ev)

#output
write.csv(operations_final, "operations/lithiumSupplyChainMapData.csv", row.names = FALSE,na='')

# Dec 2022 update -------------------------------------------------------
#load latest map data
operations <- read_csv("operations/operations_final.csv")

#save dated version as backup
write_csv(operations,paste("operations/archive/operations_final_",Sys.Date(),".csv",sep=""))

#load updated naatbatt data
path <- "operations/operations_naatbatt_Dec2022.xlsx"
df_list <- map(set_names(excel_sheets(path)),
               read_excel, path=path)
df_list_trim <- df_list[grep("^[0-9]",names(df_list))]
operations_new <- grep("^[0-9]",names(df_list), value=TRUE) %>% 
  set_names() %>%
  map_df(~read_excel(path=path,sheet=.x,col_types="text",na=c("","NA","N/A","DNA"),trim_ws=TRUE),.id = "sheet") %>%
  filter(is.na(ID)==0)
names(operations_new) <- make.names(names(operations_new))

#clean updated naatbatt data
operations_new_clean <- operations_new %>% 
  rename("Workforce"="Facility.Workforce",
         "Website"="Facility.or.Company.Website") %>%
  mutate(Production.Capacity = case_when(is.na(Production.Capacity) ~ Capacity,
                                         TRUE ~ Production.Capacity),
         Annual.Production.Units = case_when(is.na(Annual.Production.Units) ~ Production.Units,
                                             TRUE ~ Annual.Production.Units),
         Annual.Production.Units = case_when(is.na(Annual.Production.Units) ~ Capacity.Units,
                                             TRUE ~ Annual.Production.Units),
         Updated=convertToDate(QC.Date),
         Facility.Workforce=na_if(Facility.Workforce," "),
         Workforce_dummy=as.integer(Facility.Workforce),
         Workforce_dummy=replace_na(Workforce_dummy,0),
         Facility.Workforce=replace_na(Facility.Workforce,"Unknown"),
         Status=recode(Status,
                       "C"="Active","c"="Active",
                       "PC-SU"="Active",
                       "UC"="Under construction",
                       "P"="Planned","p"="Planned"),
         Status2=recode(Status,
                        "Under construction"="Planned or under construction",
                        "Planned"="Planned or under construction"),
         Supply.Chain.Segment=recode(sheet,
                                     "1-RawMatl"="Raw Materials",
                                     "2-Battery Grade Materials"="Battery Grade Materials",
                                     "3-Other Battery Comps Matls"="Other Battery Components and Materials",
                                     "4-Electrodes and Cells"="Electrodes and Cells",
                                     "5-ModPack"="Modules and Packs",
                                     "6-EOL"="End of Life",
                                     "7-Equipment"="Equipment",
                                     "8-ServiceRepair"="Service and Repair",
                                     "9-RandD"="Research and Development",
                                     "10-Modeling"="Modeling",
                                     "11-Distributors"="Distributor"),
         Data.Source="https://www.nrel.gov/transportation/li-ion-battery-supply-chain-database.html",
         Website = case_when(str_starts(Website,c("https://www.","http://www.")) ~ Website,
                             str_starts(Website,"www.") ~ paste("https://",Website,sep=""),
                             TRUE ~ paste("https://www.",Website,sep=""))) %>%
  unite("Production.Capacity","Annual.Production.Units",sep=" ",col="Production.Capacity",na.rm=TRUE) %>%
  select(c("ID",
           "Supply.Chain.Segment",
           "Updated",
           "Status",
           "Status2",
           "Company",
           "Facility.Name",
           "Facility.Type",
           "Product.Type",
           "Product",
           "Workforce",
           "Workforce_dummy",
           "Production.Capacity",
           "Website",
           "Facility.Phone",
           "Facility.City",
           "Facility.State.or.Province",
           "Facility.Country",
           "Latitude",
           "Longitude",
           "Data.Source"))
names(operations_new_clean) <-str_replace_all(names(operations_new_clean),"[.]", " ")

#add new entries to full dataset
operations_new_clean_subset <- operations_new_clean %>%
  filter(Updated>"2022-08-28")
operations_final <- bind_rows(operations,operations_new_clean_subset)

#output
write_csv(operations_final,"lithiumSupplyChainMapData.csv")

# June 2023 update -------------------------------------------------------
#load latest map data
operations <- read_csv("lithiumSupplyChainMapData.csv")

#save dated version as backup
write_csv(operations,paste("archive/lithiumSupplyChainMapData_",Sys.Date(),".csv",sep=""))

#load updated naatbatt data
path <- "operations/operations_naatbatt_June2023.xlsx"
df_list <- map(set_names(excel_sheets(path)),
               read_excel, path=path)
df_list_trim <- df_list[grep("^[0-9]",names(df_list))]
operations_new <- grep("^[0-9]",names(df_list), value=TRUE) %>% 
  set_names() %>%
  map_df(~read_excel(path=path,sheet=.x,col_types="text",na=c("","NA","N/A","DNA"),trim_ws=TRUE),.id = "sheet") %>%
  filter(is.na(ID)==0)
names(operations_new) <- make.names(names(operations_new))

#clean updated naatbatt data
operations_new_clean <- operations_new %>% 
  rename("Workforce"="Facility.Workforce",
         "Website"="Facility.or.Company.Website") %>%
  mutate(Production.Capacity = case_when(is.na(Production.Capacity) ~ Capacity,
                                         TRUE ~ Production.Capacity),
         Production.Units = case_when(is.na(Production.Units) ~ Capacity.Units,
                                      TRUE ~ Production.Units),
         Updated=convertToDate(QC.Date),
         Facility.Workforce=na_if(Facility.Workforce," "),
         Workforce_dummy=as.integer(Facility.Workforce),
         Workforce_dummy=replace_na(Workforce_dummy,0),
         Facility.Workforce=replace_na(Facility.Workforce,"Unknown"),
         Status2=recode(Status,
                        "C"="Active", "c"="Active",
                        "PC-SU"="Active",
                        "UC"="Planned or under construction",
                        "P"="Planned or under construction"),
         Status=recode(Status,
                       "C"="Commercial","c"="Commercial",
                       "PC-SU"="Pre-commercial or startup",
                       "UC"="Under construction",
                       "P"="Planned","p"="Planned"),
         Supply.Chain.Segment=recode(sheet,
                                     "1-RawMatl"="Raw Materials",
                                     "2-Battery Grade Materials"="Battery Grade Materials",
                                     "3-Other Battery Comps Matls"="Other Battery Components and Materials",
                                     "4-Electrodes and Cells"="Electrodes and Cells",
                                     "5-ModPack"="Modules and Packs",
                                     "6-EOL"="End of Life",
                                     "7-Equipment"="Equipment",
                                     "8-ServiceRepair"="Service and Repair",
                                     "9-RandD"="Research and Development",
                                     "10-Modeling"="Modeling",
                                     "11-Distributors"="Distributor"),
         Data.Source="https://www.nrel.gov/transportation/li-ion-battery-supply-chain-database.html",
         Website = case_when(str_starts(Website,c("https://www.","http://www.")) ~ Website,
                             str_starts(Website,"www.") ~ paste("https://",Website,sep=""),
                             TRUE ~ paste("https://www.",Website,sep=""))) %>%
  unite("Production.Capacity","Production.Units",sep=" ",col="Production.Capacity",na.rm=TRUE) %>%
  select(c("ID",
           "Supply.Chain.Segment",
           "Updated",
           "Status",
           "Status2",
           "Company",
           "Facility.Name",
           "Facility.Type",
           "Product.Type",
           "Product",
           "Workforce",
           "Workforce_dummy",
           "Production.Capacity",
           "Website",
           "Facility.Phone",
           "Facility.City",
           "Facility.State.or.Province",
           "Facility.Country",
           "Latitude",
           "Longitude",
           "Data.Source"))
names(operations_new_clean) <-str_replace_all(names(operations_new_clean),"[.]", " ")

#add new entries to full dataset
operations_new_clean_subset <- operations_new_clean %>%
  filter(Updated>"2022-12-04")
operations_final <- bind_rows(operations,operations_new_clean_subset)

#output
write_csv(operations_final,"lithiumSupplyChainMapData.csv")


