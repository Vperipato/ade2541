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
## Response curves ========================================================================
df_response = read.csv('./Modelo_Guido/ModelOutputs/01_IPPModel_ResponseCurves.csv')

df_response$variable = gsub("1. Mean temperature of warmest quarter",
                            "1. Mean temperature of warmest quarter (ºC)", df_response$variable)
df_response$variable = gsub("2. Precipitation of wettest quarter",
                            "2. Precipitation of wettest quarter (mm)", df_response$variable)
df_response$variable = gsub("3. Precipitation of driest quarter",
                            "3. Precipitation of driest quarter (mm)", df_response$variable)
df_response$variable = gsub("4. Soil cation concentration",
                            "4. Soil cation concentration (log10)", df_response$variable)
df_response$variable = gsub("5. Clay", "5. Clay content (g/kg)", df_response$variable)
df_response$variable = gsub("6. Silt", "6. Silt content (g/kg)", df_response$variable)
df_response$variable = gsub("7. TPI", "7. Topographic Position Index", df_response$variable)
df_response$variable = gsub("8. HAND", "8. Height Above Nearest Drainage (m)", df_response$variable)
df_response$variable = gsub("9. Water distance", "9. Water distance (m)", df_response$variable)

df_response$o = factor(df_response$variable)
levels(df_response$o) = levels(factor(sapply(str_split(df_response$o, "\\. "), "[", 1)))
df_response$variable_numb = as.numeric(df_response$o) 

df_response$variable_name = sapply(str_split(df_response$variable, "\\. "), "[", 2)
df_response$variable_name = factor(df_response$variable_name)


library(viridis)
library(viridisLite)

pretty_lim <- function(x,n=3){
  #pretty_lim
  r2 <- ifelse(x <0, 0, x)
  pr <- pretty(r2,n)
  r_out <- range(pr)
  r_out
}

pretty_unexpanded <- function(x,n=3){
  #expand_limit
  if(x[1]<=0){
    r2 <- x + c(-x[1],x[1])
  }else{
    r2 <- x + c((x[2]-x[1])*0.04545455,-(x[2]-x[1])*0.04545455)
  }
  pout <-  pretty(r2,n)
  pout
}



df_response <- df_response %>%
  mutate(variable_name = recode_factor(variable_name,
                                "Mean temperature of warmest quarter (ºC)" = "bold(Mean~temperature~of~warmest~quarter~(ºC))",
                                "Precipitation of wettest quarter (mm)" = "bold(Precipitation~of~wettest~quarter~(mm))",
                                "Precipitation of driest quarter (mm)" = "bold(Precipitation~of~driest~quarter~(mm))",
                                "Soil cation concentration (log10)" = "bold(Soil~cation~concentration~(log[10]~'[X]'))",
                                "Clay content (g/kg)" = "bold(Clay~content~(g~'/'~kg))",
                                "Silt content (g/kg)" = "bold(Silt~content~(g~'/'~kg))",
                                "Topographic Position Index" = "bold(Topographic~Position~Index)",
                                "Height Above Nearest Drainage (m)" = "bold(Height~Above~Nearest~Drainage~(m))",
                                "Water distance (m)" = "bold(Distance~to~nearest~river~(m))"))



options(scipen=999)
p_respcurves =
  ggplot(df_response, aes(x = val_x, y = val_y, color = quad))+
  geom_line(size=.5) +
  scale_colour_manual(values = c("#21918c", "#440154", "#fde725"), 
                      name = "Quartiles: ", labels = c("25%", "97.5%", "mean")) +
  scale_x_continuous(name='Response of presence probability to covariate data',
                     labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                     expand = c(0,0)) +
  scale_y_continuous(name="Presence probability in a 1km² cell",
                     labels = function(x) base::format(x, big.mar= ",", small.mark = ".", scientific = F),
                     limits = pretty_lim,
                     breaks = pretty_unexpanded,
                     expand = c(0,0)) +
  labs(subtitle = 'Intensity covariates response curves') +
  facet_wrap(reorder(variable_name, variable_numb)~., scales = "free", labeller = label_parsed) +
  theme_bw()+
  theme(panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2.5, "pt"),
        plot.margin = unit(c(0, 5, 0, 1), "pt"),
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        strip.text = element_text(size = 4.5, color = "black",
                                  margin = margin(0, 0, 0, 0, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        plot.subtitle = element_text(size = 5, face = 'bold', margin = margin(3, 0, 2, 0, "pt")),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.text.x = element_text(size = 5, colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(0, 0, 0, 0, "pt")),
        axis.text.y = element_text(size = 5, colour = 'black', margin = margin(0, 0, 0, 0, "pt")),
        axis.title.x = element_text(size = 5, face = "bold", margin = margin(2, 0, 0, 0, "pt")),
        axis.title.y = element_text(size = 5, face = "bold", margin = margin(1, 0, 0, 0, "pt")),
        legend.title = element_blank(),
        legend.position = "none")

savepath_p5 = str_c("./Modelo_Guido/ModelOutputs/IPPModel05_ResponseCurves_Vcell.png")
ggsave(savepath_p5, p_respcurves, width = 12, height = 5.45,  units = "cm", dpi = 400, bg = 'white')
dev.off()

