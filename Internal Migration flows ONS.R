
# Migration inflows outflows 2018

# We define an internal migrant as someone who moves home from one geographical area to another. This may be between local authorities, regions or countries within the UK. Unlike with international migration, there is no internationally agreed definition.

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "tmaptools", "leaflet","lemon", "fingertipsR", "PHEindicatormethods", "xlsx", "data.table", "png", "grid", "gridExtra", "circlize", "tweenr", "magick", 'jsonlite'))

Areas_to_include <- c("Adur", "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing")

if(!(exists("Areas_to_include"))){
  print("There are no areas defined. Please create or load 'Areas_to_include' which is a character string of chosen areas.")
}

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

if(!file.exists("./Migration_flow/detailedestimates2018dataset1.zip")){
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2018part12019laboundaries/detailedestimates2018dataset12019laboundaries.zip", "./Migration_flow/detailedestimates2018dataset1.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2018dataset1.zip", exdir = "./Migration_flow")


download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2018part22019laboundaries/detailedestimates2018dataset22019laboundaries.zip", "./Migration_flow/detailedestimates2018dataset2.zip", mode = "wb")
unzip("./Migration_flow/detailedestimates2018dataset2.zip", exdir = "./Migration_flow")
}

# Read in the latest (2018) LA level data and add names to the codes as well as more information about which upper tier LA and region the lower tier LA (or UA) is in
Migration_flow <- read_csv("./Migration_flow/Detailed_Estimates_2018_Dataset_1_2019_LA_boundaries.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double())) %>% 
  bind_rows(read_csv("./Migration_flow/Detailed_Estimates_2018_Dataset_2_2019_LA_boundaries.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double()))) %>% 
  left_join(LAD_lookup[c("LTLA19CD", "LTLA19NM","UTLA19CD","UTLA19NM","RGN19NM")], by = c("OutLA" = "LTLA19CD")) %>% 
  rename(OutLA_name = LTLA19NM) %>% 
  rename(OutLA_upper_tier = UTLA19CD) %>% 
  rename(OutLA_upper_tier_name = UTLA19NM) %>% 
  rename(OutRegion_name = RGN19NM) %>% 
  left_join(LAD_lookup[c("LTLA19CD", "LTLA19NM","UTLA19CD","UTLA19NM","RGN19NM")], by = c("InLA" = "LTLA19CD")) %>% 
  rename(InLA_name = LTLA19NM) %>% 
  rename(InLA_upper_tier = UTLA19CD) %>% 
  rename(InLA_upper_tier_name = UTLA19NM) %>% 
  rename(InRegion_name = RGN19NM) %>% 
  mutate(Sex = ifelse(Sex == "F", "Female", ifelse(Sex == "M", "Male", NA))) %>% 
  select(Age, Sex, InLA, InLA_name, InLA_upper_tier, InLA_upper_tier_name, InRegion_name, OutLA, OutLA_name, OutLA_upper_tier, OutLA_upper_tier_name, OutRegion_name, Moves)

paste0("The overall detailed estimates dataset includes a total of ", format(nrow(Migration_flow), big.mark = ",")," records.")

# OutLA	Nine digit code for the local authority which is the origin of an internal migration flow.
# InLA	Nine digit code for the local authority which is the destination of an internal migration flow.
# Age	The age of internal migrants as at 30 June 2018.
# Moves	The number of internal migration moves within each flow.  Note that the numbers are not integers. This is because of the various scaling processes we use, which are described in more detail in the latest methodology document. 

Migration_flow_total <- Migration_flow %>% 
  group_by(OutLA, OutLA_name, OutLA_upper_tier, OutLA_upper_tier_name, OutRegion_name, InLA, InLA_name, InLA_upper_tier, InLA_upper_tier_name, InRegion_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE)) %>% 
  ungroup()


#	Aggregating total moves to upper tier LA and regions ####

# LA-level inflows and outflows may be derived directly from the dataset 
# However, inflows and outflows for groups of LAs should not simply be added together as the totals will include moves between those LAs. 

# Create a dataframe just for those moves that are out of lower tier LA district but within the same upper tier LA county
Migration_flow_total_within_UTLA <- Migration_flow_total %>% 
  filter(OutLA_upper_tier_name == InLA_upper_tier_name)

# This df should now have internal migration, but it will have some rows that need to be combined (e.g. adur has become West Sussex, but so have chichester, crawley etc.)
UTLA_Migration_flow_total <- Migration_flow_total %>% 
  filter(OutLA_upper_tier_name != InLA_upper_tier_name) %>% 
  group_by(InLA_upper_tier, InLA_upper_tier_name, InRegion_name, OutLA_upper_tier, OutLA_upper_tier_name, OutRegion_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))

# You can check that the region migration makes sense against a table produced by ONS (it still needs to be rejigged)

# Matrices ####

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland

# if(!file.exists("./Migration_flow/laandregionalsquarematrices2018newboundaries.xlsx")){
# download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/matricesofinternalmigrationmovesbetweenlocalauthoritiesandregionsincludingthecountriesofwalesscotlandandnorthernireland/yearendingjune2018/laandregionalsquarematrices2018newboundaries.xlsx", "./Migration_flow/laandregionalsquarematrices2018newboundaries.xlsx", mode = "wb")
# }
# 
# Mig_2018_region <- read_excel("./Migration_flow/laandregionalsquarematrices2018newboundaries.xlsx",sheet = "IM2018-T8", skip = 9) %>% 
#   select(-"...1") %>% 
#   rename(In_code = "...2") %>% 
#   filter(!(is.na(In_code))) %>% 
#   gather(key = Out_code, value = "Number", 2:ncol(.)) %>% 
#   mutate(Number = as.numeric(Number)) %>% 
#   filter(!(is.na(Number))) %>% 
#   left_join(LAD_lookup[c("RGN19CD", "RGN19NM")], by = c("In_code" = "RGN19CD")) %>% 
#   unique() %>% 
#   rename(In_name = RGN19NM) %>% 
#   left_join(LAD_lookup[c("RGN19CD", "RGN19NM")], by = c("Out_code" = "RGN19CD")) %>% 
#   unique() %>% 
#   rename(Out_name = RGN19NM)

region_meta <- data.frame(Region = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"), Order = seq(1,12,1), Colour = c("#bb5f70","#d3434b","#c96d34","#b58f48","#a8b23b","#588347","#5dbb61","#4db6c2","#6776c8","#a259c7","#c782bf","#cf4597"), Label_l1 = c("North East","North West","Yorkshire and", "East Midlands","West Midlands","East of", "London", "South East", "South West", "Wales", "Scotland", "Northern"), Label_l2 = c(NA,NA,"the Humber",NA,NA,"England",NA,NA,NA,NA,NA, "Ireland")) %>% 
  mutate(Region = as.character(Region)) %>% 
  mutate(Colour =  as.character(Colour)) %>% 
  mutate(Colour = ifelse(Region == "South East", paste0(Colour), paste0(Colour,30)))

Region_Migration_flow_total <- Migration_flow_total %>% 
  filter(OutRegion_name != InRegion_name) %>% 
  select(OutRegion_name, InRegion_name, Moves) %>% 
  group_by(OutRegion_name, InRegion_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(OutRegion_name = as.character(OutRegion_name)) %>% 
  mutate(InRegion_name = as.character(InRegion_name)) %>% 
  mutate(Moves = Moves/1000)


# The next plot is saved as a png file
png(file = paste0("/Users/richtyler/Documents/Repositories/Internal-migration-flows/region_2018_chordplot.png"), height = 7, width = 7, units = "in", res = 100)

circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 6, 
           points.overflow.warning = FALSE)

# Create the plot itself
chordDiagram(Region_Migration_flow_total, 
             directional = 1, 
             order = region_meta$Region, 
             grid.col = region_meta$Colour, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE)

circos.track(track.index = 1, 
            bg.border = NA, # no borders are plotted on the track.
            panel.fun = function(x, y) {
              xlim = get.cell.meta.data("xlim") 
              sector.index = get.cell.meta.data("sector.index")
              Label_l1 = region_meta %>% # collect matching name information from plot data frame (df1).
                filter(Region == sector.index) %>% 
                pull(Label_l1)
              Label_l2 = region_meta %>% 
                filter(Region == sector.index) %>% 
                pull(Label_l2)
              
# adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
circos.text(x = mean(xlim), # add text in the middle of the arch
            y = ifelse(is.na(Label_l2), 3, 4), 
            labels = Label_l1, 
            facing = "bending", 
            cex = 0.5)
              
circos.text(x = mean(xlim), 
            y = 2.75, 
            labels = Label_l2, # adds text (reg2).
            facing = "bending", 
            cex = 0.5)
              
#add axis with major and minor ticks, without flipping the axis labels in the bottom half.
circos.axis(h = "top",
            labels.cex = 0.5, 
            labels.niceFacing = FALSE, 
            labels.pos.adjust = FALSE,
            major.at = seq(0,600,100),
            minor.ticks = 10)
            })

text(x = .7, 
     y = .9, 
     pos = 4, 
     cex = 0.6, 
     labels = paste0("Units = 1,000 moves;\nEach tick represents\n10,000 moves."))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.1, 
     labels = "The flow of the\npopulation in the\nSouth East of England")

text(x = -1, 
     y = .75, 
     pos = 4, 
     cex = 1.1, 
     col = "red",
     labels = paste0('2017-18'))

dev.off()

# Using this data on an interactive d3 chart ####

# We need to reformat that data into a table matrix and also convert the data to percentage of moves as opposed to numbers

Total_moves <- sum(Region_Migration_flow_total$Moves)
  
Matrix_reg_migration_abs <- Region_Migration_flow_total %>%
  mutate(InRegion_name = factor(InRegion_name, levels = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"))) %>% 
  arrange(InRegion_name) %>% 
  mutate(Moves = round(Moves, 0)) %>%  
  spread(InRegion_name, Moves) %>% 
  mutate(Out_total = rowSums(.[, 2:ncol(.)], na.rm = TRUE)) %>% 
  replace(., is.na(.), 0) %>% # We need to make the nas 0
  mutate(OutRegion_name = factor(OutRegion_name, levels = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"))) %>% 
  arrange(OutRegion_name)

Matrix_reg_migration_abs %>% 
  select(-OutRegion_name) %>% 
  toJSON(dataframe = 'values') %>% 
  write_lines('/Users/richtyler/Documents/Repositories/Internal-migration-flows/Matrix_reg_migration.json')

Matrix_reg_migration <- Region_Migration_flow_total %>%
  mutate(InRegion_name = factor(InRegion_name, levels = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"))) %>% 
  arrange(InRegion_name) %>% 
  mutate(Moves = Moves) %>% 
  mutate(Moves = Moves/ Total_moves) %>% 
  spread(InRegion_name, Moves) %>% 
  mutate(Out_total = rowSums(.[, 2:ncol(.)], na.rm = TRUE)) %>% 
  replace(., is.na(.), 0) %>% # We need to make the nas 0
  mutate(OutRegion_name = factor(OutRegion_name, levels = c("North East","North West","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"))) %>% 
  arrange(OutRegion_name)

write.csv(Matrix_reg_migration, "/Users/richtyler/Documents/Repositories/Internal-migration-flows/Matrix_reg_migration_proportion.csv", row.names = FALSE)

# This is saying for each row, the column is where moves were TO

Matrix_reg_migration %>% 
  select(-OutRegion_name) %>% 
  toJSON(dataframe = 'values') %>% 
  write_lines('/Users/richtyler/Documents/Repositories/Internal-migration-flows/Matrix_reg_migration.json')


i = 1
Area_x <- Areas_to_include[i]

# Identify which upper tier LA and region Area_x belongs to
Area_x_lookup <- LAD_lookup %>% 
  filter(LTLA19NM == Area_x)

Area_x_out <- Migration_flow_total %>% 
  filter(OutLA_name == Area_x) %>% 
  mutate(Rank = rank(-Moves)) %>% 
  arrange(-Rank)

Area_x_in <- Migration_flow_total %>% 
  filter(InLA_name == Area_x) %>% 
  mutate(Rank = rank(-Moves)) %>% 
  arrange(-Rank)

paste0("In ", Area_x, " in 2018, there were a total of ", format(round(sum(Area_x_in$Moves),0), big.mark = ","), " moves into the area and a total of ", format(round(sum(Area_x_out$Moves),0), big.mark = ","), " moves out of the area. This gives a net migration of ", format(round(sum(Area_x_in$Moves),0) - round(sum(Area_x_out$Moves),0), big.mark = ","), ifelse(sum(Area_x_in$Moves) - sum(Area_x_out$Moves) < 0, " fewer", " additional"), " residents.")

# You could ask which are the top five destinations for area x residents to move or come from

# Area_y = "West Sussex"
# 
# UTLA_y <- UTLA_Migration_flow_total %>% 
#   filter(OutLA_upper_tier_name == Area_y | InLA_upper_tier_name == Area_y) 
# 
# # We want to group all of the Wales UTLAs together and also perhaps a group for any areas where less than say 50 moves were made into or out of for Area_y or perhaps a % cut off (e.g. any areas where less than 1% of the total moves to compensate for different sizes of areas (as 50 may be a lot for some counties and not for others))
# 
# unique(UTLA_y$InLA_upper_tier_name)