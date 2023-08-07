install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, openxlsx, sf)

rm(list = ls())

# clean acs ---------------------------------------------------------------
#load data
acs_raw <- read_csv("cejst/acs/acs_raw.csv") %>%
  filter(!row_number()==1) %>%
  select(GEO_ID | NAME | ends_with("E"))
acs_raw_usvirginislands <- read_csv("cejst/acs/acs_raw_usvirginislands.csv") %>%
  filter(!row_number()==1)

#clean acs data
acs_raw_clean <- acs_raw %>%
  rename("Name"="NAME",
         "Population"="DP05_0001E",
         "HispanicOrLatino"="DP05_0071PE",
         "NotHispanicOrLatino"="DP05_0076PE",
         "WhiteAlone"="DP05_0077PE",
         "BlackOrAfricanAmericanAlone"="DP05_0078PE",
         "AmericanIndianAndAlaskaNativeAlone"="DP05_0079PE",
         "AsianAlone"="DP05_0080PE",
         "NativeHawaiianAndOtherPacificIslanderAlone"="DP05_0081PE",
         "OtherRaceAlone"="DP05_0082PE",
         "MultiracialAlone"="DP05_0083PE") %>%
  select(GEO_ID, 
         Name, 
         Population, 
         HispanicOrLatino,
         NotHispanicOrLatino:MultiracialAlone) %>%
  mutate(across(Population:MultiracialAlone, as.numeric)) %>%
  separate(GEO_ID,
           c(NA,"Tract"),
           sep="US",
           remove=TRUE,
           convert=TRUE)

#clean acs virgin islands data-
acs_raw_usvirginislands_clean <- acs_raw_usvirginislands %>%
  rename("Name"="NAME",
         "Population"="P005001",
         "HispanicOrLatino"="P005002",
         "NotHispanicOrLatino"="P005003",
         "OneRaceAlone"="P005004",
         "BlackOrAfricanAmericanAlone"="P005005",
         "WhiteAlone"="P005006",
         "AmericanIndianAndAlaskaNativeAlone"="P005007",
         "AsianAlone"="P005008",
         "NativeHawaiianAndOtherPacificIslanderAlone"="P005009",
         "OtherRaceAlone"="P005010",
         "MultiracialAlone"="P005011") %>%
  select(-OneRaceAlone, GEO_ID:MultiracialAlone) %>%
  mutate(across(Population:MultiracialAlone,
                as.numeric),
         across(HispanicOrLatino:MultiracialAlone,
                function(x) round(x/Population*100,1)),
         across(HispanicOrLatino:MultiracialAlone,
                ~ replace(.x,.x=="NaN",NA))) %>%
  separate(GEO_ID,
           c(NA,"Tract"),
           sep="US",
           remove=TRUE,
           convert=TRUE)

acs_final <- bind_rows(acs_raw_clean,
                       acs_raw_usvirginislands_clean)

# clean cejst data --------------------------------------------------------
cejst_raw <- read_csv("cejst/cejst_raw.csv")
cdc_raw <- read_csv("cejst/cdc/cdcPLACES_2021.csv")
cdc_clean <- cdc_raw %>%
  select(TractFIPS,
         CASTHMA_CrudePrev,
         DIABETES_CrudePrev,
         CHD_CrudePrev) %>%
  rename("Tract"="TractFIPS",
         "Asthma"="CASTHMA_CrudePrev",
         "Diabetes"="DIABETES_CrudePrev",
         "CoronaryHeartDisease"="CHD_CrudePrev")

cejst_clean <- cejst_raw %>%
  select(!contains("percentile")) %>%
  select(-c("Percent Black or African American alone":"Percent other races",
            "Total threshold criteria exceeded":"Identified as disadvantaged due to tribal overlap",
            "Share of neighbors that are identified as disadvantaged",
            "Adjusted percent of individuals below 200% Federal Poverty Line",
            "Income data has been estimated based on geographic neighbor income",
            "Is there at least one Formerly Used Defense Site (FUDS) in the tract?":"There is at least one Formerly Used Defense Site (FUDS) in the tract and the tract is low income.",
            "Unemployment (percent) in 2009 (island areas) and 2010 (states and PR)",
            "Percentage households below 100% of federal poverty line in 2009 (island areas) and 2010 (states and PR)"
            )) %>%
  rename("Tract"="Census tract 2010 ID") %>%
  mutate(Tract=as.numeric(Tract))

cejst <- left_join(cejst_clean,
                   cdc_clean,
                   by="Tract") %>%
  select(-c("Current asthma among adults aged greater than or equal to 18 years",
            "Diagnosed diabetes among adults aged greater than or equal to 18 years",
            "Coronary heart disease among adults aged greater than or equal to 18 years"))
           

###merge cejst and acs data
cejst_final <- left_join(cejst,acs_final,by="Tract") %>%
  select("Tract":"State/Territory",
         Name,
         "Identified as disadvantaged":"Total population",
         Population:MultiracialAlone,
         "Percent age under 10":"Percent age over 64",
         "Is low income?",
         "Median household income as a percent of area median income",
         "Unemployment (percent)":"Percent of residents who are not currently enrolled in higher ed",
         "Expected agricultural loss rate (Natural Hazards Risk Index)":last_col(),
         -Population) %>%
  rename("Tract population"="Total population")

# clean geodatabase layer -------------------------------------------------
usa_raw <- st_read("cejst/usa.gdb","usa")
usa <- usa_raw %>%
  select(!Tract) %>%
  mutate(Tract=as.numeric(GEOID10), .keep="unused")

# final merge -------------------------------------------------------------
#join to geodatabase layer
cejst_gdb <- full_join(usa,
                       cejst_final,
                       by=c("Tract",
                            "State"="State/Territory")) %>%
  select(!c("County Name"))

names <-read_csv("cejst/cejst_names.csv")
names(cejst_gdb)<-names(names)

cejst_gdb <- cejst_gdb %>%
  mutate(across(where(is.logical), 
                ~ifelse(., "Yes", "No")),
         across(c(AgeUnder10:AgeOver64,
                  ExpectedAgriculturalLossRate:ExpectedPopulationLossRate),
                function(x) round(x*100,1)))
  
# output ------------------------------------------------------------------
write_csv(cejst_final, "cejst_final.csv")
st_write(cejst_gdb,
         "cejst/cejst_final.gpkg",
          append=FALSE)