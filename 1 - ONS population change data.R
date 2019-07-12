
# Summary analysis ####

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "tmaptools", "leaflet","lemon", "fingertipsR", "PHEindicatormethods", "xlsx", "data.table", "png", "grid", "gridExtra", "circlize", "tweenr", "magick"))

if(!file.exists("./Migration_flow")){
  dir.create("./Migration_flow")
}

if(!(file.exists("./Migration_flow/Area_Lookup_table.csv") & file.exists("./Migration_flow/Area_types_table.csv") & file.exists("./Migration_flow/LAD_to_region_lookup.csv"))){
  LAD <- read_csv(url("https://opendata.arcgis.com/datasets/a267b55f601a4319a9955b0197e3cb81_0.csv"), col_types = cols(LAD17CD = col_character(),LAD17NM = col_character(),  LAD17NMW = col_character(),  FID = col_integer()))
  
  Counties <- read_csv(url("https://opendata.arcgis.com/datasets/7e6bfb3858454ba79f5ab3c7b9162ee7_0.csv"), col_types = cols(CTY17CD = col_character(),  CTY17NM = col_character(),  Column2 = col_character(),  Column3 = col_character(),  FID = col_integer()))
  
  Lookup <- read_csv(url("https://opendata.arcgis.com/datasets/41828627a5ae4f65961b0e741258d210_0.csv"), col_types = cols(LTLA17CD = col_character(),  LTLA17NM = col_character(),  UTLA17CD = col_character(),  UTLA17NM = col_character(),  FID = col_integer()))
  # This is a lower tier LA to upper tier LA Lookup
  UA <- subset(Lookup, LTLA17NM == UTLA17NM)
  
  CCG <- read_csv(url("https://opendata.arcgis.com/datasets/4010cd6fc6ce42c29581c4654618e294_0.csv"), col_types = cols(CCG18CD = col_character(),CCG18CDH = col_skip(),CCG18NM = col_character(), FID = col_skip())) %>% 
    rename(Area_Name = CCG18NM,
           Area_Code = CCG18CD) %>% 
    mutate(Area_Type = "Clinical Commissioning Group (2018)")
  
  Region <- read_csv(url("https://opendata.arcgis.com/datasets/cec20f3a9a644a0fb40fbf0c70c3be5c_0.csv"), col_types = cols(RGN17CD = col_character(),  RGN17NM = col_character(),  RGN17NMW = col_character(),  FID = col_integer()))
  colnames(Region) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID")
  
  Region$Area_Type <- "Region"
  Region <- Region[c("Area_Code", "Area_Name", "Area_Type")]
  
  LAD <- subset(LAD, substr(LAD$LAD17CD, 1, 1) == "E")
  LAD$Area_Type <- ifelse(LAD$LAD17NM %in% UA$LTLA17NM, "Unitary Authority", "District")
  colnames(LAD) <- c("Area_Code", "Area_Name", "Area_Name_Welsh", "FID", "Area_Type")
  LAD <- LAD[c("Area_Code", "Area_Name", "Area_Type")]
  
  Counties$Area_type <- "County"
  colnames(Counties) <- c("Area_Code", "Area_Name", "Col2", "Col3", "FID", "Area_Type")
  Counties <- Counties[c("Area_Code", "Area_Name", "Area_Type")]
  
  England <- data.frame(Area_Code = "E92000001", Area_Name = "England", Area_Type = "Country")
  
  Areas <- rbind(LAD, CCG, Counties, Region, England)
  rm(LAD, CCG, Counties, Region, England, UA)
  
  Lookup <- Lookup %>% 
    rename(LTLA_area_code = LTLA17CD,
           LTLA_area_name = LTLA17NM,
           UTLA_area_code = UTLA17CD,
           UTLA_area_name = UTLA17NM) %>% 
    select(-FID)
  
  LAD_region <- read_csv("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv", col_types = cols(LAD19CD = col_character(),  LAD19NM = col_character(),  RGN19CD = col_character(),  RGN19NM = col_character(),  FID = col_double()))
  
  LAD_Lookup <- read_csv("https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv", col_types = cols(LTLA19CD = col_character(),  LTLA19NM = col_character(),  UTLA19CD = col_character(),  UTLA19NM = col_character(),  FID = col_double())) %>% 
    left_join(LAD_region[c("LAD19CD","RGN19CD", "RGN19NM")], by = c("LTLA19CD"= "LAD19CD")) %>% 
    mutate(RGN19NM = ifelse(is.na(RGN19NM), "Wales", RGN19NM)) %>% 
    select(-"FID") %>% 
    bind_rows(data.frame(LTLA19CD = c("N92000002", "S92000003"), LTLA19NM = c("Northern Ireland", "Scotland"), UTLA19CD = c("N92000002", "S92000003"), UTLA19NM = c("Northern Ireland", "Scotland"), RGN19CD = c("N92000002", "S92000003"), RGN19NM = c("Northern Ireland", "Scotland"))) %>% 
    mutate(RGN19CD = ifelse(RGN19NM == "Wales", "W92000004", RGN19CD)) %>% 
    rename(LTLA_area_code = LTLA19CD,
           LTLA_area_name = LTLA19NM,
           UTLA_area_code = UTLA19CD,
           UTLA_area_name = UTLA19NM,
           Region_code = RGN19CD,
           Region_name = RGN19NM) 
  
write.csv(LAD_Lookup, "./Migration_flow/LAD_to_region_lookup.csv", row.names = FALSE) 
write.csv(Lookup, "./Migration_flow/Area_Lookup_table.csv", row.names = FALSE)
write.csv(Areas, "./Migration_flow/Area_types_table.csv", row.names = FALSE)

rm(list = ls())
}

if(file.exists("./Migration_flow/Area_Lookup_table.csv") & file.exists("./Migration_flow/Area_types_table.csv") & file.exists("./Migration_flow/LAD_to_region_lookup.csv")){
  Lookup <- read_csv("./Migration_flow/Area_Lookup_table.csv", col_types = cols(LTLA_area_code = col_character(), LTLA_area_name = col_character(), UTLA_area_code = col_character(), UTLA_area_name = col_character()))
  Areas <- read_csv("./Migration_flow/Area_types_table.csv", col_types = cols(Area_Code = col_character(), Area_Name = col_character(), Area_Type = col_character()))
  LAD_to_region <- read_csv("./Migration_flow/LAD_to_region_lookup.csv", col_types = cols(LTLA_area_code = col_character(),  LTLA_area_name = col_character(),UTLA_area_code = col_character(), UTLA_area_name = col_character(), Region_code = col_character(),Region_name = col_character()))
}


# Note - LA boundaries changed in April 2019, some combined (e.g. Bournemouth, Christchurch and Poole (three separate authorites), combined to make a new one (with a new code)) - This may cause some turbulence

# For West Sussex and Eastwards there dont appear to be any changes and we'll use the latest boundary codes
if(!file.exists("./Migration_flow/MYEB3_summary_components_of_change_series_UK_(2018_geog19).csv")){
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2018detailedtimeseries/ukdetailedtimeseries20012018.zip", "./Migration_flow/Components_of_change_01_18.zip", mode = "wb")
unzip("./Migration_flow/Components_of_change_01_18.zip", exdir = "./Migration_flow")}

Component_change <- read_csv("./Migration_flow/MYEB3_summary_components_of_change_series_UK_(2018_geog19).csv")

# We need to do a lot of rejigging here


# There were 242 local authorities with more people moving in than out, of which 43 had a net inflow of over 10 people per 1,000. These were predominantly in the South East, South West and East of England.
# 
# In the year to mid-2018, there were 140 local authorities with more people moving out than in, of which 30 had a net outflow of more than 10 people per 1,000. Of these 31, there were 19 in London, with the rest predominantly in the south and east.
# 
# For the year to mid-2018, London as a whole had an overall net outflow of 11.7 per 1,000 people to other areas of England and Wales (Figure 8). As described in the 2017 mid-year estimates release, there is a distinctive age structure to these moves, with children (aged under 18 years) most likely to leave, followed by adults aged over 25 years. However, there was a net inflow among the 18 to 25 years population. Broadly this corresponds to families with children tending to leave London while young adults aged in their 20s tend to move to London.

# Many of the fastest-growing authorities have high net international migration
# 
# Figure 6 shows a cluster of central London boroughs having the highest levels of net international migration in the year to mid-2018. It also shows a scattering of urban centres across England, Wales and Scotland with high international migration. These tend to have large student populations, such as Coventry, Newcastle-upon-Tyne and Oxford (these areas have high numbers of population aged 18 to 24 years and can be seen in Figure 7). However, the notable pattern from the map is that most of the UK has relatively similar levels of net international migration, as was the case in mid-2017.
# 
# Internal migration for London continues to be negative
# 
# There were 242 local authorities with more people moving in than out, of which 43 had a net inflow of over 10 people per 1,000. These were predominantly in the South East, South West and East of England.
# 
# In the year to mid-2018, there were 140 local authorities with more people moving out than in, of which 30 had a net outflow of more than 10 people per 1,000. Of these 31, there were 19 in London, with the rest predominantly in the south and east.
# 
# For the year to mid-2018, London as a whole had an overall net outflow of 11.7 per 1,000 people to other areas of England and Wales (Figure 8). As described in the 2017 mid-year estimates release, there is a distinctive age structure to these moves, with children (aged under 18 years) most likely to leave, followed by adults aged over 25 years. However, there was a net inflow among the 18 to 25 years population. Broadly this corresponds to families with children tending to leave London while young adults aged in their 20s tend to move to London.

# Flows are expressed per 1,000 to allow comparison where population sizes differ.

# In every region outside London, there was a net inflow of children and of adults aged 25 to 64 years. This was also true for the 65 years and over age group, except for very small net losses in the West Midlands and the North West.

# Fewest births since 2006
# 
# The 744,000 births taking place in the year to mid-2018 are the fewest in any year since 2006. In mid-2012 the number of births peaked at 813,000 and have subsequently decreased by 69,000.
# 
# Fertility analysis is based largely on calendar year data, for example, Birth summary tables in England and Wales: 2017. The latest UK data in Vital statistics in the UK: births, deaths and marriages – 2018 update shows that in the calendar years 2012 to 2017, UK total fertility rates decreased from 1.92 children per woman to 1.74. However, the numbers of births are related to both the number of women of fertile ages as well as their levels of fertility.
# 
# Highest number of deaths in 18 years
# 
# There were 20,000 (3%) more deaths in the year to mid-2018 than in the previous year. The 623,000 deaths in the year to mid-2018 were the most since mid-2000. Since mid-2000, the population of the UK has grown by almost 7.5 million and there are 2.4 million more people aged 65 to 84 years and 489,000 more aged 85 years or over. Further analysis of mortality is available:

# Ageing: number of over-65s continues to increase faster than the rest of the population
# 
# The composition of the UK population is determined by the patterns of births, deaths and migration that have taken place in previous years. The result is that the broad age groups in the UK population are changing at different rates, with the number of those aged 65 years and over growing faster than those under 65 years of age:
#   
#   the number of children (those aged up to 15 years) increased by 7.8% to 12.6 million between 2008 and 2018
# the working age population (those aged 16 to 64 years) increased by 3.5% to 41.6 million between 2008 and 2018
# number of people aged 65 to 84 years increased by 23.0% to 10.6 million between 2008 and 2018
# the number of people aged 85 years and over increased by 22.8% to 1.6 million between 2008 and 2018
# 
# The effects of international immigration to the UK since mid-2008 are visible in the pyramid. For most ages, the peaks and troughs present in the pyramid in mid-2008 are visible in the mid-2018 data, shifted by 10 years. However, for the population aged 22 to 39 years in mid-2018, the pyramid is wider than for the same cohort 10 years previously (when they were aged 12 to 29 years). This change has been generated by net international migration adding to the population.
# 
# The population pyramid in Figure 4 is interactive, allowing you to compare the population structures of different areas and over time. This shows that the age structure of different parts of the UK can vary considerably.
# 
# For example, in Barking and Dagenham, 27% of the population were aged 0 to 15 years and 9% were aged 65 years and over, while in the newly-formed Dorset unitary authority, 16% of the population were aged 0 to 15 years and 29% were aged 65 years and over. An interactive pyramid that can be customised further is available as part of the Analysis of population estimates (APE) tool.

# Many of the fastest-growing authorities have high net international migration
# 
# Figure 6 shows a cluster of central London boroughs having the highest levels of net international migration in the year to mid-2018. It also shows a scattering of urban centres across England, Wales and Scotland with high international migration. These tend to have large student populations, such as Coventry, Newcastle-upon-Tyne and Oxford (these areas have high numbers of population aged 18 to 24 years and can be seen in Figure 7). However, the notable pattern from the map is that most of the UK has relatively similar levels of net international migration, as was the case in mid-2017.
