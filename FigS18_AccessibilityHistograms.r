rm(list = ls(globalenv()))
gc()
removeTmpFiles(h=0)
dev.off()
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

setwd("F:/LiDAR_Data/Shapefiles")

roads = raster('./RegLogistica_Variables/ObsComponents/OpenStreetMap/Amz_DistRoads1km.tif')
trees = raster('./RegLogistica_Variables/Hansen/Cliped/Amz_TreeCover20181km.tif')
shp = read.csv('./RegLogistica_Variables/00PartnersData/01_Earthworks_MergedDatasets.csv')
coordinates(shp)<-~x+y
crs(shp) = crs(roads)

aa = extract(trees, shp, df = T)
zz = extract(roads, shp, df = T)

zz$trees = aa$Amz_TreeCover20181km
names(zz)[2] = 'roads'
zz$vari = 'geoglifo'
zz$ID = NULL
zz$roads = zz$roads * 0.001
zz = na.omit(zz)

df1 = melt(zz, id.vars = c('vari'))

which(zz$roads >=100)
# zz$roads[459] = 100L

p1 =
  ggplot(zz, aes(x = roads)) +
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

p2=
  ggplot(zz, aes(x = trees)) +
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


top_row = plot_grid(p1, p2, align = "vh", nrow = 1, labels="AUTO", label_size = 8)
top_row
savepath = str_c("./SuppMaterial/FigS18_AccessibilityHistograms.png")
ggsave(savepath, top_row, width = 12, height = 5.5,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()  

