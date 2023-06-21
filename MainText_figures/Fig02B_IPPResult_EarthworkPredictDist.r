rm(list = ls(globalenv()))
gc()
removeTmpFiles(h=0)
options(encoding = "UTF-8") 
library(bayesplot)
library(raster)
library(rgdal)
library(viridis)
library(ggmap)
library(mapview)
library(ggspatial)
library(sf)
library(stringr)
library(reshape2)
library(scales)
library(cowplot)
library(gtools)
library(dplyr)
library(bayesplot)

# library(devtools)
# install_github("GuidoAMoreira/bayesPO")
library(bayesPO)

setwd("F:/ade2541-manuscript/Database")

# List .rds files of IPP model output data
files = list.files(".", pattern = '^IPP_.*.rds$', full.names = T)
po_est_all = read_rds(files, refhook = NULL)
output_array <- as.array(po_est_all); rm(po_est_all, files)

prob_1 = mcmc_areas_data(output_array, pars = 'n_Xprime', prob = 1, prob_outer = 1, point_est = "mean")
prob_0950 = mcmc_areas_data(output_array, pars = 'n_Xprime', prob = .950, prob_outer = .950, point_est = "mean")
prob_0950 = subset(prob_0950, interval == 'outer')
prob_0950$interval = 'outer_0950'

df_plot = mcmc_areas_data(output_array, pars = 'n_Xprime', point_est = "mean")
df_plot$interval = 'point'
rm(output_array)

p_probearth =
  ggplot(df_plot, aes(x=x, y=plotting_density, fill = interval)) +
  geom_area(data = subset(prob_0950, interval = 'outer_0950'), color = 'transparent') +
  geom_area(data = subset(prob_1, interval == 'point'), color = 'transparent') +
  geom_line(linetype = "solid", colour='black', size = .25, show.legend =  F) +
  scale_fill_manual(values = c("#440154", "#fde725"),
                    name = "CI: ", labels = c("95%", "mean")) +
  scale_x_continuous(limits = c(7500,30000), 
                     labels = seq(10000, 25000, by=5000), 
                     breaks = seq(10000, 25000, by=5000),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  theme_bw()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"),
        axis.line.x = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length = unit(2, "pt"),
        title = element_text(size = 7, family = 'Arial Narrow', face = 'bold'),
        plot.subtitle =  element_text(size = 5, face = 'bold'),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, color = "transparent"),
        axis.text.x = element_text(size = 6, family = 'Arial Narrow', colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(.5, 0, 1, 0, "pt")),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

savepath = str_c("./rplots/Fig02B_EarthworkPredictDist.pdf")
ggsave(savepath, p_probearth, width = 4.45, height = 2.10,  units = "cm", dpi = 400,
       bg = 'transparent', device = cairo_pdf)
dev.off()
