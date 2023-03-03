rm(list = ls(globalenv()))
gc()
removeTmpFiles(h=0)
dev.off()
options(encoding = "UTF-8") 
library(bayesplot)
library(raster)
library(rgdal)
library(viridis)
library(ggmap)
library(mapview)
library(RStoolbox)
library(ggspatial)
library(sf)
library(stringr)
library(reshape2)
library(scales)
library(cowplot)
library(gtools)
library(dplyr)

setwd("F:/LiDAR_Data/Shapefiles/RegLogistica_Variables/")


library(bayesplot)
library(bayesPO)
po_est_all <- do.call(c, lapply(
  1:4, function(i) readRDS(paste0("./Modelo_Guido/RawData/est_0.4_multi",i,".rds"))
))
output_array <- as.array(po_est_all); #rm(po_est_all)

prob_1 = mcmc_areas_data(output_array, pars = 'n_Xprime', prob = 1, prob_outer = 1, point_est = "mean")
prob_0950 = mcmc_areas_data(output_array, pars = 'n_Xprime', prob = .950, prob_outer = .950, point_est = "mean")
prob_0950 = subset(prob_0950, interval == 'outer')
prob_0950$interval = 'outer_0950'

# prob_0500 = mcmc_areas_data(output_array, pars = 'n_Xprime', prob = .05, prob_outer = .05, point_est = "mean")
# prob_0500 = subset(prob_0500, interval == 'outer')
# prob_0500$interval = 'outer_0500'

zz = mcmc_areas_data(output_array, pars = 'n_Xprime', point_est = "mean")
zz$interval = 'point'

val_min = round(min(prob_0950$x))
val_max = round(max(prob_0950$x))
val_mean = subset(prob_1, interval == 'point')
val_mean = round(mean(val_mean$x))

breaks = c(val_min, val_mean, val_max)

#probabilidade acumulada acima de 10000
# mean(zz$x > 10000)

p_probearth =
  ggplot(zz, aes(x=x, y=plotting_density, fill = interval)) +
  geom_area(data = subset(prob_0950, interval = 'outer_0950'), color = 'transparent') +
  # geom_area(data = subset(prob_0500, interval = 'outer_0500'), color = 'transparent') +
  geom_area(data = subset(prob_1, interval == 'point'), color = 'transparent') +
  geom_line(linetype = "solid", colour='black', size = .25, show.legend =  F) +
  scale_fill_manual(values = c("#440154", "#fde725"),
                    name = "CI: ", labels = c("95%", "mean")) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), expand = c(0,0)) +
  # scale_x_continuous(labels = label_number(suffix = "K", scale = 1e-3, accuracy = 1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  # labs(subtitle = 'Earthwork probability distribution') +
  theme_bw() +
  theme(plot.margin = unit(c(1, 0, 0, 0), "pt"),
        axis.line.x = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length = unit(2, "pt"),
        title = element_text(size = 7, face = 'bold'),
        plot.subtitle =  element_text(size = 5, face = 'bold'),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, color = "transparent"),
        axis.text.x = element_text(size = 6, colour = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.title.x = element_text(size = 12, face = "bold"),
        axis.title = element_blank(),
        legend.title = element_text(size=5, face = "bold"),
        legend.text = element_text(size=5),
        legend.justification=c(.5,-.5),
        legend.margin=margin(1,0,1,0),
        legend.box.margin=margin(1,0,1,0),
        legend.position = "none")
  

savepath_p2 = str_c("./Modelo_Guido/ModelOutputs/IPPModel02_EarthworkPredictDist.png")
ggsave(savepath_p2, p_probearth, width = 4.45, height = 2.10,  units = "cm", dpi = 400, bg = 'white')
dev.off()
