rm(list = ls(globalenv()))
gc()
removeTmpFiles(h=0)
dev.off()
options(encoding = "UTF-8") 
library(scales)
library(directlabels)
library(ggplot2)
library(raster)
library(spatialEco)
library(stringr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggrepel)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(likert)

color_pallete = function(n) {
  hues = seq(0, 360, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# library(jtools)
setwd("F:/LiDAR_Data/Shapefiles/RegLogistica_Variables")

## 1. ABUNDÂNCIA OU PA ##############################################################################
files = c('./00PartnersData/ATDN/RawData/ATDN_SpeciesPA_FIX.csv',
          './00PartnersData/ATDN/RawData/ATDN_SpeciesAbundance_FIX.csv')

r = raster('./Modelo_Guido/IPPModelResult_orig.tif')


for (x in 1:length(files)) {
  # Abrir .csv de Abundância  ou Presença/Ausência de espécies por parcela
  csv = read.csv(files[x], header=TRUE, sep=",")
  coordinates(csv)<-~Longitude+Latitude
  csv = raster::extract(r, csv, sp=T)
  csv = as.data.frame(csv)
  csv = na.omit(csv)
  
  if (x == 1) {
    plots_used = csv[c(1:3, ncol(csv))]
    write.csv(plots_used, file = './00PartnersData/ATDN/RawData/ATDN_Plots_Used.csv',
              row.names = F, fileEncoding = "UTF-8")
    rm(plots_used)
    }

  
  species = csv[-c(1:4, length(csv))]
  prob_geoglifo = csv$IPPModelResult_orig
  area = csv$PlotSize/100 # trasnforma a area da parcela de Hectare para Km²
  allBetas = NULL
  
  ## 1.1 Regressão (all betas data.frame) ##############################################################################
  for (i in 1:ncol(species)) {
    #x=explicativa; y=resposta 
    specie_pa = species[,i]
    if (basename(files[x]) == "ATDN_SpeciesPA_FIX.csv") {regressao = 'binomial'}
    if (basename(files[x]) == "ATDN_SpeciesAbundance_FIX.csv") {regressao = 'poisson'}
    reg = glm(specie_pa ~ log10(prob_geoglifo), offset = area, family = regressao) #binomial para PA | poisson para Abundancia
    df = data.frame('species' = str_replace(names(species[i]), "_", " "),
                    
                    'intercept' = summary.glm(reg)$coefficients[1],
                    'se' = summary.glm(reg)$coefficients[4],
                    'slope' = summary.glm(reg)$coefficients[2],
                    'AIC' = AIC(reg))
    allBetas = rbind(allBetas, df); rm(df, reg, specie_pa)
  }
  rm(species, i, area, regressao, csv)
  
  ## 1.2 Remoção de espécies especificas ##############################################################################
  #### Plotting response curves
  # Vetores ficticios/
  # allBetas # Finge que este data.frame tem todas as especies nas linhas, estimativas de intercepto, inclinacao e s.e. da inclinacao nas colunas
  # earthworkProbabilities # Finge que este vetor tem todas as prob. de presenca que te passei
  # earthworkProbabilities = readRDS("./Modelo_Guido/RawData/parcelasProbabilities.rds")
  # earthworkProbabilities = data.frame(prob = unname(earthworkProbabilities))
  # eP <- seq(min(earthworkProbabilities), max(earthworkProbabilities), len = 500)
  # eP <- seq(min(values(r), na.rm = T), max(values(r), na.rm = T), len = 500)
  eP <- seq(-8, 0, len = 1000)
  # eP <- seq(min(prob_geoglifo, na.rm = T), max(prob_geoglifo, na.rm = T), len = 500)
  rm(prob_geoglifo)
  # rm(earthworkProbabilities)
  
  # if (basename(files[x]) == "ATDN_SpeciesAbundance.csv"){
  #   ind = which(allBetas$species %in% c('Erisma japura'))
  #   allBetas = allBetas[-c(ind),]; rm(ind)}
  
  
  #### Corrigindo nivel de significancia
  desiredSignificance <- 0.05
  numberOfTests <- nrow(allBetas) #79 
  correctedSignificance <- 1 - (1 - desiredSignificance) ^ (1 / numberOfTests)
  multiplier <- qnorm(1 - correctedSignificance / 2)
  
  allBetas$range_min = allBetas$slope - (multiplier * allBetas$se)
  allBetas$range_max = allBetas$slope + (multiplier * allBetas$se)
  
  allBetas$has0between = data.table::between(0, allBetas$range_min, allBetas$range_max)
  ind = which(allBetas$has0between == TRUE)
  
  allBetas = allBetas[-c(ind), ]
  rm(ind, numberOfTests, desiredSignificance, correctedSignificance)
  
  allBetas$significance = NA
  ind=which(allBetas$range_max < 0)
  allBetas$significance[ind] = 'Negative'
  ind=which(allBetas$range_min > 0)
  allBetas$significance[ind] = 'Positive'
  rm(ind)
  
  if (basename(files[x]) == "ATDN_SpeciesPA_FIX.csv"){
    allBetas_PA = allBetas
    allBetas_PA$source = 'Presence/Absence'}
  if (basename(files[x]) == "ATDN_SpeciesAbundance_FIX.csv"){
    allBetas_Abd = allBetas
    allBetas_Abd$source = 'Abundance'}
  rm(allBetas)
}



allBetas = rbind(allBetas_PA, allBetas_Abd)
# df_plot1$intercept = NULL
# df_plot1$se = NULL
# df_plot1$AIC = NULL
# df_plot1$has0between = NULL
# df_plot1$significance = NULL

# allBetas_pos = subset(allBetas, significance == 'Positive')
# allBetas = allBetas_pos

allBetas = allBetas[order(allBetas$species, decreasing = T),]

allBetas$species = factor(allBetas$species)
allBetas$species = reverse.levels(allBetas$species)
species = as.character(unique(allBetas$species))

allBetas$species_numb = allBetas$species

levels(allBetas$species_numb) = seq(1, length(levels(factor(allBetas$species))), by=1)
allBetas$species_numb = as.numeric(as.character(allBetas$species_numb))
allBetas$species = as.character(allBetas$species)


allBetas$source = factor(allBetas$source, levels = c('Presence/Absence', 'Abundance'))
allBetas$source = recode_factor(allBetas$source, 'Presence/Absence'='Occurrence', 'Abundance' = 'Abundance')
allBetas$significance = factor(allBetas$significance, levels = c('Positive', 'Negative'))
# allBetas_neg = subset(allBetas, significance == 'Negative')


## 2. GGPLOTS!! ##############################################################################
# 2.1 Plot all effects ##############################################################################
p_boxplot =
  ggplot(allBetas, aes(x=slope, y=species_numb)) +
  facet_grid(significance~source, scales = 'free_y', space = 'free')+
  geom_vline(xintercept=0, alpha = 1, linetype="solid", color = "#21918c", size=1.5) +
  geom_segment(aes(y=species, x = range_min,
                   yend = species, xend = range_max), colour = '#440154', size = 3) +
  # geom_boxplot(outlier.alpha = 0) +
  stat_summary(aes(y=species, x=slope), fun=mean, geom="point", size=1.5, col='#fde725', shape=18)+
  scale_x_continuous("Earthwork probability coefficient",
                     limits = c(-4.5, 4.5), breaks = seq(-10, 10, by = 1), expand = c(0, 0)) +
  
  # "Earthwork probability coefficient log10(x)", limits = c(-3.5, 3.5), breakscale_x_continuous(name = 'Earthwork probability distribution', expand = c(0,0)s = seq(-10, 10, by = 1), expand = c(0, 0)) +
  labs(#title = str_c(df_plots$ggtitle),
    subtitle = 'Point estimate and confidence interval',
    # caption = str_c("Overall significance level of 5%")
  )+
  # facet_wrap(~ source, scales="free", ncol=2)+
  # facet_rep_grid(~source, scales = 'free_y', repeat.tick.labels = c('all')) +
  
  scale_y_discrete(position = c("left"), limits=rev) +
  # scale_y_continuous(breaks = seq(1, length(unique(allBetas$species)), by=1),
  #                    labels = unique(allBetas$species),
  #                    minor_breaks = NULL,
  #                    oob = rescale_none,
  #                    expand = c(0,0.5),
  #                    sec.axis = dup_axis()) +
  theme_bw() +
  theme(panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2.5, "pt"),
        plot.margin = unit(c(1, 1, 1, 1), "pt"), 
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        strip.text = element_text(size = 7, color = "black", face = "bold",
                                  margin = margin(1, 1, 2, 2, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        # plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 7, face = 'bold', margin = margin(2, 0, 2, 0, "pt")),
        plot.caption = element_text(face = "italic", size = 10, hjust=0.5),
        # title = element_blank(),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.title.x = element_text(size = 7, face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7, angle = 0, face = "italic", vjust = .25),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")


savepath = str_c("./00PartnersData/ATDN/RawData/rplots/Fig03_DomesticatedBoxplot.png")
ggsave(savepath, p_boxplot, width = 12, height = 18,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()
