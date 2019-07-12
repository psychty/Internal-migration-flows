
# Migration inflows outflows 2018

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "tmaptools", "leaflet","lemon", "fingertipsR", "PHEindicatormethods", "xlsx", "data.table", "png", "grid", "gridExtra", "circlize", "tweenr", "magick"))

LAD_region <- read_csv("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv", col_types = cols(LAD19CD = col_character(),  LAD19NM = col_character(),  RGN19CD = col_character(),  RGN19NM = col_character(),  FID = col_double()))

LAD_lookup <- read_csv("https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv", col_types = cols(LTLA19CD = col_character(),  LTLA19NM = col_character(),  UTLA19CD = col_character(),  UTLA19NM = col_character(),  FID = col_double())) %>% 
  left_join(LAD_region[c("LAD19CD","RGN19CD", "RGN19NM")], by = c("LTLA19CD"= "LAD19CD")) %>% 
  mutate(RGN19NM = ifelse(is.na(RGN19NM), "Wales", RGN19NM)) %>% 
  select(-"FID") %>% 
  bind_rows(data.frame(LTLA19CD = c("N92000002", "S92000003"), LTLA19NM = c("Northern Ireland", "Scotland"), UTLA19CD = c("N92000002", "S92000003"), UTLA19NM = c("Northern Ireland", "Scotland"), RGN19CD = c("N92000002", "S92000003"), RGN19NM = c("Northern Ireland", "Scotland"))) %>% 
  mutate(RGN19CD = ifelse(RGN19NM == "Wales", "W92000004", RGN19CD))

rm(LAD_region)

if(!file.exists("./Migration_flow")){
  dir.create("./Migration_flow")
}

if(!file.exists("./Migration_flow/detailedestimates2017dataset1.zip")){
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2017part1/detailedestimates2017dataset1.zip", "./Migration_flow/detailedestimates2017dataset1.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2017dataset1.zip", exdir = "./Migration_flow")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2017part2/detailedestimates2017dataset2.zip", "./Migration_flow/detailedestimates2017dataset2.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2017dataset2.zip", exdir = "./Migration_flow")

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2018part12019laboundaries/detailedestimates2018dataset12019laboundaries.zip", "./Migration_flow/detailedestimates2018dataset1.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2018dataset1.zip", exdir = "./Migration_flow")


download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2018part22019laboundaries/detailedestimates2018dataset22019laboundaries.zip", "./Migration_flow/detailedestimates2018dataset2.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2018dataset2.zip", exdir = "./Migration_flow")
}

Migration_flow <- read_csv("./Migration_flow/Detailed_Estimates_2018_Dataset_1_2019_LA_boundaries.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double())) %>% 
  bind_rows(read_csv("./Migration_flow/Detailed_Estimates_2018_Dataset_2_2019_LA_boundaries.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double())))

Migration_flow <- left_join(Migration_flow, LAD_lookup[c("LTLA19CD", "LTLA19NM")], by = c("OutLA" = "LTLA19CD")) %>% 
  rename(OutLA_name = LTLA19NM)

# All OutLAs are matched to a name
table(is.na(Migration_flow$OutLA_name))

Migration_flow <- left_join(Migration_flow, LAD_lookup[c("LTLA19CD", "LTLA19NM")], by = c("InLA" = "LTLA19CD")) %>% 
  rename(InLA_name = LTLA19NM)

# All InLAs are matched to a name
table(is.na(Migration_flow$InLA_name))

Migration_flow <- Migration_flow[c("OutLA", "OutLA_name", "InLA", "InLA_name", "Age","Sex","Moves")]

paste0("The overall detailed estimates dataset includes a total of ", format(nrow(Migration_flow), big.mark = ",")," records.")

# OutLA	Nine digit code for the local authority which is the origin of an internal migration flow.
# InLA	Nine digit code for the local authority which is the destination of an internal migration flow.
# Age	The age of internal migrants as at 30 June 2018.
# Sex	The sex of internal migrants.
# Data coding used within the dataset:

# Moves	The number of internal migration moves within each flow.  Note that the numbers are not integers. This is because of the various scaling processes we use, which are described in more detail in the latest methodology document. 

# Age: Defined as age as at 30 June 2017, so in many cases will be one year older than age at actual move.

Migration_flow_total <- Migration_flow %>% 
  group_by(OutLA, OutLA_name, InLA, InLA_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))

#	Aggregating figures: LA-level inflows and outflows may be derived directly from the dataset.
# However, inflows and outflows for groups of LAs should not simply be added together as the totals will include moves between those LAs. Users will therefore need to take care to strip out those moves. However, the release includes ready-made tables of total moves at regional level. 

Migration_flow_total <- Migration_flow_total %>% 
  left_join(LAD_lookup[c("LTLA19CD", "UTLA19NM","RGN19NM")], by = c("OutLA" = "LTLA19CD")) %>% 
  rename(OutLA_upper_tier_name = UTLA19NM) %>% 
  rename(OutRegion_name = RGN19NM) %>% 
  left_join(LAD_lookup[c("LTLA19CD", "UTLA19NM","RGN19NM")], by = c("InLA" = "LTLA19CD")) %>% 
  rename(InLA_upper_tier_name = UTLA19NM) %>% 
  rename(InRegion_name = RGN19NM) %>% 
  ungroup()

# Create a dataframe just for those moves that are out of lower tier LA district but within the same upper tier LA county
Migration_flow_total_within_UTLA <- Migration_flow_total %>% 
  filter(OutLA_upper_tier_name == InLA_upper_tier_name)

# This df should now have internal migration, but it will have some rows that need to be combined (e.g. adur has become West Sussex, but so have chichester, crawley etc.)
UTA_Migration_flow_total <- Migration_flow_total %>% 
  filter(OutLA_upper_tier_name != InLA_upper_tier_name) %>% 
  select(OutLA_upper_tier_name, InLA_upper_tier_name, Moves) %>% 
  group_by(OutLA_upper_tier_name, InLA_upper_tier_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))

# Region_Migration_flow_total <- subset(Migration_flow_total, OutRegion_name != InRegion_name)

Region_Migration_flow_total <- Migration_flow_total %>% 
  filter(OutRegion_name != InRegion_name) %>% 
  select(OutRegion_name, InRegion_name, Moves) %>% 
  group_by(OutRegion_name, InRegion_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE)) %>% 
  mutate(Year = "2018")

# You can check that the region migration makes sense against a table produced by ONS (it still needs to be rejigged)

# Matrices ####

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland/yearendingjune2018/laandregionalsquarematrices2018newboundaries.xlsx", "./Migration_flow/laandregionalsquarematrices2018newboundaries.xlsx", mode = "wb")

Mig_2018_region <- read_excel("./Migration_flow/laandregionalsquarematrices2018newboundaries.xlsx",sheet = "IM2018-T8", skip = 9) %>% 
  select(-"...1") %>% 
  rename(In_code = "...2") %>% 
  filter(!(is.na(In_code))) %>% 
  gather(key = Out_code, value = "Number", 2:ncol(.)) %>% 
  mutate(Number = as.numeric(Number)) %>% 
  filter(!(is.na(Number))) %>% 
  left_join(LAD_lookup[c("RGN19CD", "RGN19NM")], by = c("In_code" = "RGN19CD")) %>% 
  unique() %>% 
  rename(In_name = RGN19NM) %>% 
  left_join(LAD_lookup[c("RGN19CD", "RGN19NM")], by = c("Out_code" = "RGN19CD")) %>% 
  unique() %>% 
  rename(Out_name = RGN19NM)

# Hurrah, adjusting for the rounding, our estimates of those leaving East of England for East Midlands is 26983.6198 and the ONS table is 26984

# Fix this # xmax option in chordDiagram that can be used to fix the lengths of the x-axis for each sector using a named vector. In the context of producing an animation, the historic maximum migration flows (of combined immigration and emigration flows) in each region can be used, calculated from the original data d0

