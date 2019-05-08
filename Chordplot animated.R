
#install.packages("migest")

library(migest)
library(tidyverse)
library(magrittr)
library(tweenr)
library(magick)


d0 <- read_csv(system.file("imr", "reg_flow.csv", package = "migest"))

#View(d0) 

# xmax option in chordDiagram that can be used to fix the lengths of the x-axis for each sector using a named vector. In the context of producing an animation, the historic maximum migration flows (of combined immigration and emigration flows) in each region can be used, calculated from the original data d0

reg_max <- d0 %>%
  group_by(year0, orig_reg) %>%
  mutate(tot_out = sum(flow)) %>%
  group_by(year0, dest_reg) %>%
  mutate(tot_in = sum(flow)) %>%
  filter(orig_reg == dest_reg) %>%
  mutate(tot = tot_in + tot_out) %>%
  mutate(reg = orig_reg) %>%
  group_by(reg) %>%
  summarise(tot_max = max(tot)/1e06) %$%
  'names<-'(tot_max, reg)

d1 <- read_csv(system.file("vidwp", "reg_plot.csv", package = "migest"))
#View(d1)

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
