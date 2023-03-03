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
library(extrafont)

setwd("F:/LiDAR_Data/Shapefiles/RegLogistica_Variables/")
## Contribuição individual de cada variavel ========================================================================
df_contrib = read.csv('./Modelo_Guido/ModelOutputs/00_IPPModel_CovarianceImportance.csv')
df_contrib$vari = gsub("Intensity", "Intensity", df_contrib$vari)
df_contrib$vari = gsub("Observability", "Obs.", df_contrib$vari)

df_contrib$variable = gsub("1. Mean temperature\nwarmest quarter",
                           "01. Mean temperature of warmest quarter", df_contrib$variable)
df_contrib$variable = gsub("2. Precipitation\nwettest quarter",
                           "02. Precipitation of wettest quarter", df_contrib$variable)
df_contrib$variable = gsub("3. Precipitation\ndriest quarter",
                           "03. Precipitation of driest quarter", df_contrib$variable)
df_contrib$variable = gsub("4. Soil cation\nconcentration",
                           "04. Soil cation concentration", df_contrib$variable)
df_contrib$variable = gsub("5. Clay",
                           "05. Clay content", df_contrib$variable)
df_contrib$variable = gsub("6. Silt",
                           "06. Silt content", df_contrib$variable)
df_contrib$variable = gsub("7. TPI",
                           "07. Topographic Position Index", df_contrib$variable)
df_contrib$variable = gsub("8. HAND",
                           "08. Height Above Nearest Drainage", df_contrib$variable)
df_contrib$variable = gsub("9. Water\ndistance",
                           "09. Water distance", df_contrib$variable)
df_contrib$variable = gsub("1. Roads\ndistance",
                           "10. Roads distance", df_contrib$variable)
df_contrib$variable = gsub("2. Tree\ncover",
                           "11. Tree cover", df_contrib$variable)

df_contrib$o = factor(df_contrib$variable)
levels(df_contrib$o) = levels(factor(sapply(str_split(df_contrib$o, "\\. "), "[", 1)))
df_contrib$variable_numb = as.numeric(df_contrib$o) 

df_contrib$variable_name = sapply(str_split(df_contrib$variable, "\\. "), "[", 2)
df_contrib$variable_name = factor(df_contrib$variable_name)


df_contrib$variable_name = gsub("Mean temperature of warmest quarter",
                                "Mean temp. of warmest quarter", df_contrib$variable_name)

df_contrib$variable_name = gsub("Water distance",
                                "Distance to nearest river", df_contrib$variable_name)

df_contrib$variable_name = gsub("Roads distance",
                                "Distance to nearest road", df_contrib$variable_name)

p_varicontrib =
  ggplot(df_contrib, aes(x = value, y = reorder(variable_name, desc(variable_numb)))) +
  geom_boxplot(fill='#440154', width = .95, lty = 1, lwd = 0.05,
               outlier.shape = 4, outlier.size = .5, outlier.alpha = 0.1,
               color = '#21918c', alpha = 1) +
  stat_summary(fun=mean, geom="point", size=0.95, col='#fde725', shape=18) +
  scale_y_discrete(name="Variable") +
  scale_x_continuous(name = 'Relative contribution', oob = rescale_none, limits = c(0, 1),
                     labels = percent,
                     breaks = seq(0, 1, by=.25),
                     expand = c(0,0)) + 
  facet_grid(vari~., scales = "free", space = "free") +
  labs(subtitle = 'Covariate contribution') +
  theme_bw()+
  theme(panel.spacing = unit(1, "pt"),
        plot.margin = unit(c(0, 1, 0, 0.0), "pt"),
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(0, "pt"),
        strip.text = element_text(size = 5, color = "black",
                                    margin = margin(0, 0, 0, 1.5, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        plot.subtitle = element_text(size = 5, face = 'bold', margin = margin(3, 0, 2, 0, "pt")),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.text.x = element_text(size = 5, colour = 'black', margin = margin(0, 1, 0, 0, "pt")),
        axis.text.y = element_text(size = 4.5, colour = 'black', margin = margin(0, .5, 0, 0, "pt")),
        # axis.title.x = element_text(size = 4.5, face = "bold", margin = margin(0, 0, 0, 0, "pt")),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

# x11()
# print(p_varicontrib)

savepath_p4 = str_c("./Modelo_Guido/ModelOutputs/IPPModel04_VariableContribution.png")
ggsave(savepath_p4, p_varicontrib, width = 4.45, height = 3.85,  units = "cm", dpi = 400, bg = 'white')
dev.off()

