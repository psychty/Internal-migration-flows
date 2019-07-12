# Chordplot 

# This code is adapted from Guy Abel's excellent tutorial on creating an animated chord plot of global migration.

# I have editted the code to make it more familiar to my own workflow (which is a step I encourage you to try to learn and use new code). The original post is available here: https://guyabel.com/post/animated-directional-chord-diagrams/

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "tmaptools", "leaflet","lemon", "fingertipsR", "PHEindicatormethods", "xlsx", "data.table", "png", "grid", "gridExtra", "circlize", "tweenr", "magick", "gtools"))

d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"), col_types = cols(  year0 = col_double(),  orig_reg = col_character(),  dest_reg = col_character(),  flow = col_double())) %>%
  mutate(corridor = paste0(orig_reg, " -> ", dest_reg)) 

# Step one - add a new variable describing the start and end area
# Step two - keep only the desrcibing variable, the year, and the number of people

d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))

# This creates larger data frame d2, with 100 observations for each corridor, one for each frame in the animation. In the original data d0 there are only 11 observations for each corridor, one for each five-year period.

d0_expanded <- d0 %>%
  select(corridor, year0, flow) %>% 
  mutate(ease = "linear") %>% # We need to create a field holding the 'easing function' name
  tween_elements(time = "year0", # tween_elements essentially creates x number of frames to show a transition between the years
                 group = "corridor", 
                 ease = "ease", 
                 nframes = 100) %>%
  separate(col = .group, into = c("orig_reg", "dest_reg"), sep = " -> ") %>% # we need to split the corridor label back into two fields showing origin and desination
  select(orig_reg, dest_reg, flow, everything()) %>% # This says as long as orig_reg, dest_reg and flow are first, the rest of the fields can be as they were
  mutate(flow = flow/1000000)

reg_max_1 <-  d0 %>%
  group_by(year0, orig_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(orig_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = orig_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)/1000000) 

reg_max <- reg_max_1$tot_max
names(reg_max) <- reg_max_1$reg

rm(reg_max_1)

# Now the data is in the correct format, chord diagrams can be produced for each frame of the eventual GIF. 

# Lets start with plotting the latest datapoint
frame_x <- max(d0_expanded$.frame, na.rm = TRUE)

# The chordplot fails when fields other than the origin, destination and number are included in the dataframe. As such we need to strip any extra fields out (but keep a record of what they were elsewhere)

latest <- d0_expanded %>% 
  filter(.frame == frame_x) %>% 
  select(orig_reg, dest_reg, flow, year0)

year_x = floor(unique(latest$year0))

latest <- latest %>% 
  select(-year0)

circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 4, 
           points.overflow.warning = FALSE)

chordDiagram(latest, # Create the plot itself
             directional = 1, 
             order = d1$region, 
             grid.col = d1$col1, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             xmax = reg_max)

circos.track(track.index = 1, # First line indicates that first track (rather than any other that may have been created) will be used.
             bg.border = NA, # Second line ensures no borders are plotted on the track.
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim") # collect individual track meta data from plot object.
               sector.index = get.cell.meta.data("sector.index")
               reg1 = d1 %>% # collect matching name information from plot data frame (df1).
                 filter(region == sector.index) %>% 
                 pull(reg1)
               reg2 = d1 %>% 
                 filter(region == sector.index) %>% 
                 pull(reg2)
               
               circos.text(x = mean(xlim), # add text in the middle of the arch
                           y = ifelse(is.na(reg2), 3, 4), # adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
                           labels = reg1, 
                           facing = "bending", 
                           cex = 0.8)
               
               circos.text(x = mean(xlim), 
                           y = 2.75, 
                           labels = reg2, # adds text (reg2).
                           facing = "bending", 
                           cex = 0.8)
               
               circos.axis(h = "top", #add axis with major and minor ticks, without flipping the axis labels in the bottom half.
                           labels.cex = 0.6, 
                           labels.niceFacing = FALSE, 
                           labels.pos.adjust = FALSE)
             })

text(x = -1, 
     y = -0.975, 
     pos = 4, 
     cex = 0.6, 
     labels = "Based on estimates from:")

text(x = -1, 
     y = -1 - 1*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(plain("Abel G.J. (2016) "), italic("Estimates of Global Bilateral Migration Flows by Gender")))) # You can use expression(paste()) to change formatting mid way through a string

text(x = -1, 
     y = -1 - 2*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(italic("between 1960 and 2015. "), plain("Vienna Institute of Demography Working Papers. 2/2016"))))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.2, 
     labels = "The global flow of\nthe population")

text(x = -1, 
     y = .8, 
     pos = 4, 
     cex = 1.2, 
     col = "red",
     labels = paste0(year_x))

text(x = .7, 
     y = .975, 
     pos = 4, 
     cex = 0.8, 
     labels = paste0("Units = millions"))

# The next plot is saved as a png file
png(file = paste0("./Migration_flow/demo_global_chordplot.png"), height = 7, width = 7, units = "in", res = 100)

circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 4, 
           points.overflow.warning = FALSE)

chordDiagram(latest, # Create the plot itself
             directional = 1, 
             order = d1$region, 
             grid.col = d1$col1, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             xmax = reg_max)

circos.track(track.index = 1, # First line indicates that first track (rather than any other that may have been created) will be used.
             bg.border = NA, # Second line ensures no borders are plotted on the track.
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim") # collect individual track meta data from plot object.
               sector.index = get.cell.meta.data("sector.index")
               reg1 = d1 %>% # collect matching name information from plot data frame (df1).
                 filter(region == sector.index) %>% 
                 pull(reg1)
               reg2 = d1 %>% 
                 filter(region == sector.index) %>% 
                 pull(reg2)
               
               circos.text(x = mean(xlim), # add text in the middle of the arch
                           y = ifelse(is.na(reg2), 3, 4), # adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
                           labels = reg1, 
                           facing = "bending", 
                           cex = 0.8)
               
               circos.text(x = mean(xlim), 
                           y = 2.75, 
                           labels = reg2, # adds text (reg2).
                           facing = "bending", 
                           cex = 0.8)
               
               circos.axis(h = "top", #add axis with major and minor ticks, without flipping the axis labels in the bottom half.
                           labels.cex = 0.6, 
                           labels.niceFacing = FALSE, 
                           labels.pos.adjust = FALSE)
             })

text(x = -1, 
     y = -0.975, 
     pos = 4, 
     cex = 0.6, 
     labels = "Based on estimates from:")

text(x = -1, 
     y = -1 - 1*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(plain("Abel G.J. (2016) "), italic("Estimates of Global Bilateral Migration Flows by Gender")))) # You can use expression(paste()) to change formatting mid way through a string

text(x = -1, 
     y = -1 - 2*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(italic("between 1960 and 2015. "), plain("Vienna Institute of Demography Working Papers. 2/2016"))))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.2, 
     labels = "The global flow of\nthe population")

text(x = -1, 
     y = .8, 
     pos = 4, 
     cex = 1.2, 
     col = "red",
     labels = paste0(year_x))

text(x = .7, 
     y = .975, 
     pos = 4, 
     cex = 0.8, 
     labels = paste0("Units = millions"))

dev.off()


# Now we have got this working we want to automate a loop through each datapoint

# To do this, loop through the tweened data. 

# create a directory to store the individual plots
if(!file.exists("./Migration_flow/demo_chordplot_images/")){
dir.create("./Migration_flow/demo_chordplot_images/")}


for(i in 1:max(d0_expanded$.frame)){
  
frame_x = i
  
df_x <- d0_expanded %>% 
  filter(.frame == frame_x) %>% 
  select(orig_reg, dest_reg, flow, year0)

year_x = floor(unique(df_x$year0))

df_x <- df_x %>% 
  select(-year0)

# The next plot is saved as a png file
png(file = paste0("./Migration_flow/demo_chordplot_images/demo_plot_", i, ".png"), height = 7, width = 7, units = "in", res = 70)

circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 90, 
           track.margin=c(-0.2, 0.2),
           gap.degree = 4, 
           points.overflow.warning = FALSE)

chordDiagram(df_x, # Create the plot itself
             directional = 1, 
             order = d1$region, 
             grid.col = d1$col1, 
             annotationTrack = "grid", 
             transparency = 0.25,  
             annotationTrackHeight = c(0.05, 0.1), 
             direction.type = c("diffHeight", "arrows"), 
             link.arr.type = "big.arrow",
             diffHeight  = -0.02, 
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             xmax = reg_max)

circos.track(track.index = 1, # First line indicates that first track (rather than any other that may have been created) will be used.
             bg.border = NA, # Second line ensures no borders are plotted on the track.
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim") # collect individual track meta data from plot object.
               sector.index = get.cell.meta.data("sector.index")
               reg1 = d1 %>% # collect matching name information from plot data frame (df1).
                 filter(region == sector.index) %>% 
                 pull(reg1)
               reg2 = d1 %>% 
                 filter(region == sector.index) %>% 
                 pull(reg2)
               
               circos.text(x = mean(xlim), # add text in the middle of the arch
                           y = ifelse(is.na(reg2), 3, 4), # adds text from (reg1) either at y = 4 (if there is a second part of the name in reg2) or 3.
                           labels = reg1, 
                           facing = "bending", 
                           cex = 0.8)
               
               circos.text(x = mean(xlim), 
                           y = 2.75, 
                           labels = reg2, # adds text (reg2).
                           facing = "bending", 
                           cex = 0.8)
               
               circos.axis(h = "top", #add axis with major and minor ticks, without flipping the axis labels in the bottom half.
                           labels.cex = 0.6, 
                           labels.niceFacing = FALSE, 
                           labels.pos.adjust = FALSE)
             })

text(x = -1, 
     y = -0.975, 
     pos = 4, 
     cex = 0.6, 
     labels = "Based on estimates from:")

text(x = -1, 
     y = -1 - 1*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(plain("Abel G.J. (2016) "), italic("Estimates of Global Bilateral Migration Flows by Gender")))) # You can use expression(paste()) to change formatting mid way through a string

text(x = -1, 
     y = -1 - 2*0.03, 
     pos = 4, 
     cex = 0.6,
     labels = expression(paste(italic("between 1960 and 2015. "), plain("Vienna Institute of Demography Working Papers. 2/2016"))))

text(x = -1, 
     y = .9, 
     pos = 4, 
     cex = 1.2, 
     labels = "The global flow of\nthe population")

text(x = -1, 
     y = .8, 
     pos = 4, 
     cex = 1.2, 
     col = "red",
     labels = paste0(year_x))

text(x = .7, 
     y = .975, 
     pos = 4, 
     cex = 0.8, 
     labels = paste0("Units = millions"))

dev.off()
}

# Image processing ####

# lift.files is putting them alphanumerically (1, 10, 100, 11 and so on rather than 1,2,3,4 etc). As such we need to reorder the list.files command using the 'mixedsort' function from gtools

mixedsort(sort(list.files(path = paste0("./Migration_flow/demo_chordplot_images"), pattern = "*.png", full.names = T))) %>% 
    map(image_read) %>% # reads each path file
    image_join() %>% # joins image
    image_animate(fps = 4) %>% # animates, can opt for number of loops
    image_write(paste0("./Migration_flow/demo_chordplot.gif"),
                quality = 50,
                density = 100)


