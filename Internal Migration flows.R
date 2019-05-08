
# Migration inflows outflows
library(tidyverse)

LAD_lookup <- read_csv("https://opendata.arcgis.com/datasets/41828627a5ae4f65961b0e741258d210_0.csv")

LAD_region <- read_csv("https://opendata.arcgis.com/datasets/c457af6314f24b20bb5de8fe41e05898_0.csv", col_types = cols(LAD17CD = col_character(),LAD17NM = col_character(),RGN17CD = col_character(),RGN17NM = col_character(),FID = col_integer()))

LAD_lookup <- left_join(LAD_lookup, LAD_region[c("LAD17CD","RGN17CD", "RGN17NM")], by = c("LTLA17CD"= "LAD17CD"))

LAD_lookup$RGN17NM <- ifelse(is.na(LAD_lookup$RGN17NM), "Wales", LAD_lookup$RGN17NM)

LAD_lookup$FID <- NULL
rm(LAD_region)

# download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2017part1/detailedestimates2017dataset1.zip", "./Migration_flow/detailedestimates2017dataset1.zip", mode = "wb")
# unzip("./Migration_flow/detailedestimates2017dataset1.zip", exdir = "./Migration_flow")
# 
# download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset/yearendingjune2017part2/detailedestimates2017dataset2.zip", "./Migration_flow/detailedestimates2017dataset2.zip", mode = "wb")
# unzip("./Migration_flow/detailedestimates2017dataset2.zip", exdir = "./Migration_flow")

Part_1 <- read_csv("./Migration_flow/Detailed_Estimates_2017_Dataset_1.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double()))
Part_2 <- read_csv("./Migration_flow/Detailed_Estimates_2017_Dataset_2.csv", col_types = cols(OutLA = col_character(),InLA = col_character(),Age = col_integer(),Sex = col_character(),Moves = col_double()))

Migration_flow <- rbind(Part_1, Part_2)
rm(Part_1, Part_2)

LAD_lookup <- rbind(LAD_lookup, data.frame(LTLA17CD = c("N92000002", "S92000003"), LTLA17NM = c("Northern Ireland", "Scotland"), UTLA17CD = c("N92000002", "S92000003"), UTLA17NM = c("Northern Ireland", "Scotland"), RGN17CD = NA, RGN17NM = c("Northern Ireland", "Scotland")))

Migration_flow <- left_join(Migration_flow, LAD_lookup[c("LTLA17CD", "LTLA17NM")], by = c("OutLA" = "LTLA17CD"))
# All OutLAs are matched to a name
table(is.na(Migration_flow$LTLA17NM))
# Change the name to OutLA_name
names(Migration_flow)[names(Migration_flow) == 'LTLA17NM'] <- 'OutLA_name'

Migration_flow <- left_join(Migration_flow, LAD_lookup[c("LTLA17CD", "LTLA17NM")], by = c("InLA" = "LTLA17CD"))
# All InLAs are matched to a name
table(is.na(Migration_flow$LTLA17NM))
# Change the name to InLA_name
names(Migration_flow)[names(Migration_flow) == 'LTLA17NM'] <- 'InLA_name'


Migration_flow <- Migration_flow[c("OutLA", "OutLA_name", "OutRegion_name", "InLA", "InLA_name", "InRegion_name", "Age","Sex","Moves")]

# The overall detailed estimates dataset includes a total of 1,371,941 records.

# The separate sections of the detailed estimates dataset included in the two zip files contain the following number of records: Part 1 = 862,703 and Part 2 = 509,238.

# OutLA	Nine digit code for the local authority which is the origin of an internal migration flow.
# InLA	Nine digit code for the local authority which is the destination of an internal migration flow.
# Age	The age of internal migrants as at 30 June 2017.
# Sex	The sex of internal migrants.
# Data coding used within the dataset:

# Moves	The number of internal migration moves within each flow.  Note that the numbers are not integers. This is because of the various scaling processes we use, which are described in more detail in the latest methodology document. 

# Age: Defined as age as at 30 June 2017, so in many cases will be one year older than age at actual move.

Migration_flow_total <- Migration_flow %>% 
  group_by(OutLA, OutLA_name, InLA, InLA_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))

#	Aggregating figures: LA-level inflows and outflows may be derived directly from the dataset.
# However, inflows and outflows for groups of LAs should not simply be added together as the totals will include moves between those LAs. Users will therefore need to take care to strip out those moves. However, the release includes ready-made tables of total moves at regional level. 

Migration_flow_total <- left_join(Migration_flow_total, LAD_lookup[c("LTLA17CD", "UTLA17NM","RGN17NM")], by = c("OutLA" = "LTLA17CD"))
# All InLAs are matched to a name
table(is.na(Migration_flow_total$UTLA17NM))
# Change the name to InLA_name
names(Migration_flow_total)[names(Migration_flow_total) == 'UTLA17NM'] <- 'OutLA_upper_tier_name'
names(Migration_flow_total)[names(Migration_flow_total) == 'RGN17NM'] <- 'OutRegion_name'

Migration_flow_total <- left_join(Migration_flow_total, LAD_lookup[c("LTLA17CD", "UTLA17NM","RGN17NM")], by = c("InLA" = "LTLA17CD"))
# All InLAs are matched to a name
table(is.na(Migration_flow_total$UTLA17NM))
# Change the name to InLA_name
names(Migration_flow_total)[names(Migration_flow_total) == 'UTLA17NM'] <- 'InLA_upper_tier_name'
names(Migration_flow_total)[names(Migration_flow_total) == 'RGN17NM'] <- 'InRegion_name'

Migration_flow_total_within_UTLA <- subset(Migration_flow_total, OutLA_upper_tier_name == InLA_upper_tier_name)

# This df should now have internal migration, but it will have some rows that need to be combined (e.g. adur has become West Sussex, but so have chichester, crawley etc.)
UTA_Migration_flow_total <- subset(Migration_flow_total, OutLA_upper_tier_name != InLA_upper_tier_name)
UTA_Migration_flow_total <- UTA_Migration_flow_total[c("OutLA_upper_tier_name", "InLA_upper_tier_name", "Moves")] %>% 
  group_by(OutLA_upper_tier_name, InLA_upper_tier_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))

# Region_Migration_flow_total <- subset(Migration_flow_total, OutRegion_name != InRegion_name)

Region_Migration_flow_total <- Migration_flow_total

Region_Migration_flow_total <- Region_Migration_flow_total[c("OutRegion_name", "InRegion_name", "Moves")] %>% 
  group_by(OutRegion_name, InRegion_name) %>% 
  summarise(Moves = sum(Moves, na.rm = TRUE))


# Chordplot####

library(migest)
library(tidyverse)
library(magrittr)
library(tweenr)
library(magick)


d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"))

View(d0) 

Region_Migration_flow_total$Year <- "2017"

# xmax option in chordDiagram that can be used to fix the lengths of the x-axis for each sector using a named vector. In the context of producing an animation, the historic maximum migration flows (of combined immigration and emigration flows) in each region can be used, calculated from the original data d0

reg_max <-  Region_Migration_flow_total %>%
  group_by(Year, OutRegion_name) %>%
  mutate(tot_out = sum(Moves)) %>%
  group_by(Year, InRegion_name) %>%
  mutate(tot_in = sum(Moves)) %>%
  filter(OutRegion_name == InRegion_name) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = OutRegion_name) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)/1e06) %$%
  'names<-'(tot_max, reg)

d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))
View(d1)

# This creates larger data frame d2, with 100 observations for each corridor, one for each frame in the animation. In the original data d0 there are only 11 observations for each corridor, one for each five-year period.

d2 <- d0 %>%
  mutate(corridor = paste(orig_reg, dest_reg, sep = " -> ")) %>%
  select(corridor, year0, flow) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year0", group = "corridor", ease = "ease", nframes = 100) %>%
  tbl_df()

#View(d2)

# Then some further minor data wrangling is required to ready the data for plotting using the chordDiagram function; namely the first three columns in the data must correspond to the origin, destination and flow.

d2 <- d2 %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>%
  select(orig_reg, dest_reg, flow, everything()) %>%
  mutate(flow = flow/1e06)

#View(d2)

# Now the data is in the correct format, chord diagrams can be produced for each frame of the eventual GIF. To do this, I used a for loop to cycle through the tweend data. The arguments I used in the circos.par, chordDiagram and circos.track functions to produce each plot are explained in more detail in the comments of the migest demo.

# create a directory to store the individual plots
dir.create("./plot-gif/")

#install.packages('circlize')
library(circlize)

for(f in unique(d2$.frame)){
  # open a PNG plotting device
  png(file = paste0("./plot-gif/globalchord", f, ".png"), height = 7, width = 7, units = "in", res = 500)
  
  # intialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex=1)
  circos.par(start.degree = 90, track.margin=c(-0.1, 0.1),gap.degree = 4, points.overflow.warning = FALSE)
  
  # plot the chord diagram
  chordDiagram(x = filter(d2, .frame == f), directional = 1, order = d1$region, grid.col = d1$col1, annotationTrack = "grid", transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1), direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow",diffHeight  = -0.04, link.sort = TRUE, link.largest.ontop = TRUE, xmax = reg_max)
  
  # add labels and axis
  circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = d1 %>% filter(region == sector.index) %>% pull(reg1)
    reg2 = d1 %>% filter(region == sector.index) %>% pull(reg2)
    circos.text(x = mean(xlim), y = ifelse(is.na(reg2), 3, 4),labels = reg1, facing = "bending", cex = 1.1)
    circos.text(x = mean(xlim), y = 2.75, labels = reg2, facing = "bending", cex = 1.1)
    circos.axis(h = "top", labels.cex = 0.8, labels.niceFacing = FALSE, labels.pos.adjust = FALSE)
  })
  
  # close plotting device
  dev.off()
}

img <- image_read(path = "./plot-gif/globalchord0.png")
for(f in unique(d2$.frame)[-1]){
  img0 <- image_read(path = paste0("./plot-gif/globalchord",f,".png"))
  img <- c(img, img0)
  message(f)
}

img1 <- image_scale(image = img, geometry = "720x720")

ani0 <- image_animate(image = img1, fps = 10)
image_write(image = ani0, path = "./plot-gif/globalchord.gif")
