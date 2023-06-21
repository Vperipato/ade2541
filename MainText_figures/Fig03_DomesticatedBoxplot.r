rm(list = ls(globalenv()))
gc()
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

setwd("F:/ade2541-manuscript/Database")

# List .rds files of Abundance and Ocurrence regression data
files = list.files('.', pattern = '.rds$', full.names = T)

for (k in seq_along(files)) {
  reg = read_rds(files[k], refhook = NULL)
  allBetas = NULL
  
  for (i in 1:ncol(reg)) {
    specie = reg[,i]
    specie_name = strsplit(paste(specie$call)[2], '\\=|\\ ~')[[1]][1]
    
    df = data.frame('species' = str_replace(specie_name, "_", " "),
                    'intercept' = summary.glm(specie)$coefficients[1],
                    'se' = summary.glm(specie)$coefficients[4],
                    'slope' = summary.glm(specie)$coefficients[2])
    allBetas = rbind(allBetas, df)
    rm(specie, specie_name, df)
    
  }
  rm(reg)
  
  #### Correct significance level
  desiredSignificance <- 0.05
  numberOfTests <- nrow(allBetas) #79 species
  correctedSignificance <- 1 - (1 - desiredSignificance) ^ (1 / numberOfTests)
  multiplier <- qnorm(1 - correctedSignificance / 2)
  
  allBetas$range_min = allBetas$slope - (multiplier * allBetas$se)
  allBetas$range_max = allBetas$slope + (multiplier * allBetas$se)
  rm(multiplier)
  
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
  
  if (basename(files[k]) == "GLMReg_SpeciesAbundance.rds"){
    allBetas_Abd = allBetas
    allBetas_Abd$source = 'Abundance'}
  if (basename(files[k]) == "GLMReg_SpeciesOcurrence.rds"){
    allBetas_PA = allBetas
    allBetas_PA$source = 'Presence/Absence'}
  rm(allBetas)
}
allBetas = rbind(allBetas_PA, allBetas_Abd)
rm(allBetas_PA, allBetas_Abd, i, k, files)

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

## 2. GGPLOTS!! ##############################################################################
# 2.1 Plot point estimate | Occurrence & Abundance | Positive & Negative #####################
p_boxplot =
  ggplot(allBetas, aes(x=slope, y=species_numb)) +
  facet_grid(significance~source, scales = 'free_y', space = 'free')+
  geom_vline(xintercept=0, alpha = 1, linetype="solid", color = "#21918c", size=1.5) +
  geom_segment(aes(y=species, x = range_min,
                   yend = species, xend = range_max), colour = '#440154', size = 3) +
  stat_summary(aes(y=species, x=slope), fun=mean, geom="point", size=1.5, col='#fde725', shape=18)+
  scale_x_continuous("Earthwork probability coefficient",
                     limits = c(-4.5, 4.5), breaks = seq(-10, 10, by = 1), expand = c(0, 0)) +
  labs(subtitle = 'Point estimate and confidence interval')+
  scale_y_discrete(position = c("left"), limits=rev) +
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
        plot.subtitle = element_text(size = 7, face = 'bold', margin = margin(2, 0, 2, 0, "pt")),
        plot.caption = element_text(face = "italic", size = 10, hjust=0.5),
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

savepath = str_c("./rplots/Fig03_DomesticatedBoxplot.png")
ggsave(savepath, p_boxplot, width = 12, height = 18,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()
