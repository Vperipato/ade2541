rm(list = ls(globalenv()))
gc()
options(encoding = "UTF-8") 
library(raster)
library(raster)
library(rgdal)
library(ggplot2)
library(scales)
library(reshape2)
library(viridis)
library(cowplot)
library(stringr)

setwd("F:/ade2541-manuscript/Database/")

# Load Earthworks presence data
samples = read_rds('./Earthworks.rds', refhook = NULL)
# Transform to spatial data
coordinates(samples)<-~Longitude+Latitude
crs(samples) = CRS("+init=epsg:4326")

## 1.1 Road Distance Histogram #########################################################
# Check SM for raster data
roads = raster('./Rasters/OpenStreetMap/DistanceToRoads.tif')
hist_roads = extract(roads, samples, df = T)
names(hist_roads)[2] = 'roads'
# transform values from m to km
hist_roads$roads = hist_roads$roads * 0.001

# ggplot
p_roads = 
  ggplot(hist_roads, aes(x = roads)) +
  geom_histogram(color="#fde725", fill="#440154", binwidth = 5, size=.25, boundary = 0) +
  scale_fill_viridis()+
  scale_x_continuous(name="Distance from earthwork to nearest road (km)",
                     limits = c(0,100), breaks = seq(0, 100, by=20), 
                     oob = rescale_none, expand = c(0,0)) +
  
  scale_y_continuous(name = 'Frequency', 
                     limits =c(0,900), breaks = seq(0, 900, by=100),
                     oob = rescale_none, expand = c(0,0)) + 
  ggtitle('Distance from earthwork to nearest road') +
  theme_bw() +
  theme(panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2.5, "pt"),
        plot.margin = unit(c(5, 15, 1, 5), "pt"),
        title = element_blank(),
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(size = 7, colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(0, 0, 0, 0, "pt")),
        axis.text.y = element_text(size = 7, colour = 'black', margin = margin(0, 0, 0, 0, "pt")),
        axis.title.x = element_text(size = 7, face = "bold", margin = margin(2, 0, 0, 0, "pt")),
        axis.title.y = element_text(size = 7, face = "bold", margin = margin(1, 0, 0, 0, "pt")),
        legend.title = element_blank(),
        legend.position = "none")
rm(roads, hist_roads)


## 1.2 Tree Cover Histogram #########################################################
# Check SM for raster data
trees = raster('./Rasters/GFC/GFC2018_v1.6.tif')
hist_trees = extract(trees, samples, df = T)
names(hist_trees)[2] = 'trees'

# ggplot
p_trees=
  ggplot(hist_trees, aes(x = trees)) +
  geom_histogram(color="#fde725", fill="#440154", binwidth = 5, size=.25, boundary = 0) +
  scale_fill_viridis()+
  scale_x_continuous(name="Tree cover over earthwork (%)",
                     limits = c(0,100), breaks = seq(0, 100, by=20), 
                     oob = rescale_none, expand = c(0,0)) +
  scale_y_continuous(name = 'Frequency', limits =c(0,250),
                     oob = rescale_none, expand = c(0,0)) +
  ggtitle('Tree cover over earthwork') +
  theme_bw() +
  theme(panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2.5, "pt"),
        plot.margin = unit(c(10, 10, 1, 5), "pt"),
        title = element_blank(),
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(size = 7, colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(0, 0, 0, 0, "pt")),
        axis.text.y = element_text(size = 7, colour = 'black', margin = margin(0, 0, 0, 0, "pt")),
        axis.title.x = element_text(size = 7, face = "bold", margin = margin(2, 0, 0, 0, "pt")),
        axis.title.y = element_text(size = 7, face = "bold", margin = margin(1, 0, 0, 0, "pt")),
        legend.title = element_blank(),
        legend.position = "none")
rm(trees, hist_trees)
rm(samples)

## 1.3 Merge two plots into single #########################################################
p_final = plot_grid(p_roads, p_trees, align = "vh", nrow = 1, labels="AUTO", label_size = 8)
rm(p_roads, p_trees)
savepath = str_c("./rplots/sm/FigS18_AccessibilityHistograms.png")
ggsave(savepath, p_final, width = 12, height = 5.5,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()
