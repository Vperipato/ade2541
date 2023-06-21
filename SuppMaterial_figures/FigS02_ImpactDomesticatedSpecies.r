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

color_pallete = function(n) {
  hues = seq(0, 360, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

setwd("F:/ade2541-manuscript/Database")

# List .rds files of Abundance and Ocurrence regression data
files = list.files('.', pattern = '^GLMReg_.*.rds$', full.names = T)

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
rm(files, k, i)

## 2. GGPLOTS!! ##############################################################################
# 2.1 Plot Impact | Occurrence | Positive & Negative #########################################
eP <- seq(-8, 0, len = 1000)
percent = .1  #10% more positive & negative output 
allBetas_PA_plot = NULL
for (i in 1:nrow(allBetas_PA)){
  thisRow <- allBetas_PA[i, ]
  prob = exp(-log1p(exp(-(thisRow$intercept + thisRow$slope * eP))))
  temp.df = data.frame(eP = eP, prob = prob, species = str_replace(thisRow$species, "_", " "),
                       significance = thisRow$significance, source = thisRow$source)
  allBetas_PA_plot = rbind(allBetas_PA_plot, temp.df)
  rm(thisRow, temp.df, prob)
}
#color setup
allBetas_PA_plot = allBetas_PA_plot[order(allBetas_PA_plot$species, decreasing = F),] #allBetas_PA_plot
allBetas_PA = allBetas_PA[order(allBetas_PA$species, decreasing = F),] #allBetas_PA
color = data.frame('species' = allBetas_PA$species,
                   'significance' = allBetas_PA$significance,
                   'source' = allBetas_PA$source,
                   'prob_min' = subset(allBetas_PA_plot, eP == min(eP))[,2],
                   'prob_max' = subset(allBetas_PA_plot, eP == max(eP))[,2])

color$color = 'grey'
color$o = 0

len_positive = round(length(which(color$significance == 'Positive')) * percent)
len_negative = round(length(which(color$significance == 'Negative')) * percent)

top_positive = order(color$prob_max, decreasing = T)[1:len_positive]
top_negative = order(color$prob_min, decreasing = T)[1:len_negative]

ind = c(top_positive, top_negative)
color$color[ind] = color_pallete(length(ind))
color$o[ind] = 1
rm(ind, top_positive, top_negative, len_positive, len_negative)

allBetas_PA_plot = merge(allBetas_PA_plot, color[c(1,2,3,6,7)], by=c('species', 'significance', 'source'))
allBetas_PA_plot$o = as.factor(apply(format(allBetas_PA_plot[,c("o", "species")]), 1, paste, collapse=" "))
allBetas_PA_plot$source = recode_factor(allBetas_PA_plot$source, 'Presence/Absence'='Occurrence', 'Abundance' = 'Abundance')

p_impact_pa =
  ggplot(data = allBetas_PA_plot, aes(x = eP, y = prob, color = species, group = o)) +
  geom_line(data = subset(allBetas_PA_plot, color == 'grey'),
            size=1, alpha = .4, show.legend = FALSE) +
  geom_line(data = subset(allBetas_PA_plot, color != 'grey'),
            size=1, alpha = 1, show.legend = FALSE) +
  geom_point(data = subset(allBetas_PA_plot, significance == 'Negative' & eP == min(eP) & color != 'grey'),
             size = 3, show.legend = FALSE, shape=19) +
  geom_point(data = subset(allBetas_PA_plot, significance == 'Positive' & eP == max(eP) & color != 'grey'),
             size = 3, show.legend = FALSE, shape=19) + 
  scale_colour_manual(values = color$color) +
  facet_wrap(.~source,  scales="free", ncol=1) +
  scale_x_continuous("Earthwork occurrence probability",
                     limits = c(-9.3, 1.5),
                     breaks = seq(-8, 0, by=1),
                     labels = c(expression(10^-8),
                                expression(10^-7),
                                expression(10^-6),
                                expression(10^-5),
                                expression(10^-4),
                                expression(10^-3),
                                expression(10^-2),
                                expression(10^-1),
                                expression(1)),
                     oob = rescale_none,
                     expand = c(0,0),
                     minor_breaks = seq(-8, 0, by=.5)) +
  scale_y_continuous("Impact on domesticated species", limits = c(0,1), expand = c(0,0)) +
  geom_label_repel(
    data = subset(allBetas_PA_plot, eP == min(eP) & significance == 'Negative' & color != 'grey'),
    aes(label = species),
    ylim = c(0, 1),
    seed = 1,
    size = 2.5,
    box.padding = unit(0.1, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size=.5, 
    show.legend = FALSE,
    fontface = "italic",
    direction         = "both",
    min.segment.length = 20,
    segment.linetype  = NA,
    segment.size      = 0.5,
    segment.curvature = 1,
    segment.angle = 0,
    segment.inflect = T,
    max.overlaps = Inf)+
  geom_label_repel(
    data = subset(allBetas_PA_plot, eP == max(eP) & significance == 'Positive' & color != 'grey'),
    aes(label = species),
    ylim = c(0, 1),
    seed = 1,
    size = 2.5,
    box.padding = unit(0.1, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size=.5, 
    show.legend = FALSE,
    fontface = "italic",
    direction         = "both",
    min.segment.length = 20,
    segment.linetype  = NA,
    segment.size      = 0.5,
    segment.curvature = 1,
    segment.angle = 0,
    segment.inflect = T,
    max.overlaps = Inf)+
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 1, 10), "pt"), 
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        strip.text = element_text(size = 7, color = "black", face = "bold",
                                  margin = margin(1, 1, 2, 2, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = 'black', face = 'bold'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

# 2.1 Plot Impact | Abundance | Positive & Negative #########################################
allBetas_Abd_plot = NULL
for (i in 1:nrow(allBetas_Abd)){
  thisRow <- allBetas_Abd[i, ]
  prob = exp(thisRow$intercept + thisRow$slope * eP)
  temp.df = data.frame(eP = eP, prob = prob, species = str_replace(thisRow$species, "_", " "),
                       significance = thisRow$significance, source = thisRow$source)
  allBetas_Abd_plot = rbind(allBetas_Abd_plot, temp.df)
  rm(thisRow, temp.df, prob)
}

#color setup
allBetas_Abd_plot = allBetas_Abd_plot[order(allBetas_Abd_plot$species, decreasing = F),] #allBetas_Abd_plot
allBetas_Abd = allBetas_Abd[order(allBetas_Abd$species, decreasing = F),] #allBetas_Abd
color = data.frame('species' = allBetas_Abd$species,
                   'significance' = allBetas_Abd$significance,
                   'source' = allBetas_Abd$source,
                   'prob_min' = subset(allBetas_Abd_plot, eP == min(eP))[,2],
                   'prob_max' = subset(allBetas_Abd_plot, eP == max(eP))[,2])

color$color = 'grey'
color$o = 0

len_positive = round(length(which(color$significance == 'Positive')) * percent)  
len_negative = round(length(which(color$significance == 'Negative')) * percent) 

top_positive = order(color$prob_max, decreasing = T)[1:len_positive]
top_negative = order(color$prob_min, decreasing = T)[1:len_negative]

ind = c(top_positive, top_negative)
color$color[ind] = color_pallete(length(ind))
color$o[ind] = 1
rm(ind, top_positive, top_negative, len_positive, len_negative)

allBetas_Abd_plot = merge(allBetas_Abd_plot, color[c(1,2,3,6,7)], by=c('species', 'significance', 'source'))
allBetas_Abd_plot$o = as.factor(apply(format(allBetas_Abd_plot[,c("o", "species")]), 1, paste, collapse=" "))

p_impact_abd =
  ggplot(data = allBetas_Abd_plot, aes(x = eP, y = prob, color = species, group = o)) +
  geom_line(data = subset(allBetas_Abd_plot, color == 'grey'),
            size=1, alpha = .4, show.legend = FALSE) +
  geom_line(data = subset(allBetas_Abd_plot, color != 'grey'),
            size=1, alpha = 1, show.legend = FALSE) +
  geom_point(data = subset(allBetas_Abd_plot, significance == 'Negative' & eP == min(eP) & color != 'grey'),
             size = 3, show.legend = FALSE, shape=19) +
  geom_point(data = subset(allBetas_Abd_plot, significance == 'Positive' & eP == max(eP) & color != 'grey'),
             size = 3, show.legend = FALSE, shape=19) + 
  scale_colour_manual(values = color$color) +
  facet_wrap(.~source,  scales="free", ncol=1) +
  scale_x_continuous("Earthwork occurrence probability",
                     limits = c(-9.3, 1.5),
                     breaks = seq(-8, 0, by=1),
                     labels = c(expression(10^-8),
                                expression(10^-7),
                                expression(10^-6),
                                expression(10^-5),
                                expression(10^-4),
                                expression(10^-3),
                                expression(10^-2),
                                expression(10^-1),
                                expression(1)),
                     oob = rescale_none,
                     expand = c(0,0),
                     minor_breaks = seq(-8, 0, by=.5)) +
  scale_y_continuous("Impact on domesticated species", limits = c(0,40), expand = c(0,0)) +
  geom_label_repel(
    data = subset(allBetas_Abd_plot, source == 'Abundance' & eP == min(eP) & significance == 'Negative' & color != 'grey'),
    aes(label = species),
    seed = 1,
    size = 2.5,
    box.padding = unit(0.1, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size=.5, 
    show.legend = FALSE,
    fontface = "italic",
    direction         = "both",
    min.segment.length = 20,
    segment.linetype  = NA,
    segment.size      = 0.5,
    segment.curvature = 1,
    segment.angle = 0,
    segment.inflect = T,
    max.overlaps = Inf)+
  geom_label_repel(
    data = subset(allBetas_Abd_plot, source == 'Abundance' & eP == max(eP) & significance == 'Positive' & color != 'grey'),
    aes(label = species),
    seed = 1,
    size = 2.5,
    box.padding = unit(0.1, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size=.5,
    show.legend = FALSE,
    fontface = "italic",
    direction         = "both",
    min.segment.length = 20,
    segment.linetype  = NA,
    segment.size      = 0.5,
    segment.curvature = 1,
    segment.angle = 0,
    segment.inflect = T,
    max.overlaps = Inf) +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 1, 10), "pt"), 
        axis.line = element_line(colour = "black", size = 0.05, linetype = "solid"),
        axis.ticks = element_line(color="black", size = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        axis.ticks.length.y = unit(1, "pt"),
        strip.text = element_text(size = 7, color = "black", face = "bold",
                                  margin = margin(1, 1, 2, 2, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7, color = 'black', face = 'bold'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

library(cowplot)
p_impact = plot_grid(p_impact_pa, p_impact_abd, ncol=1, align="vh", labels = 'AUTO', label_size = 10)
p_impact = grid.arrange(p_impact,
                        bottom = textGrob("Earthwork occurrence probability",
                                          gp = gpar(fontsize = 7, fontface="bold"),
                                          hjust = 0.5))

savepath = str_c("./rplots/Fig04_ImpactDomesticatedSpecies.png")
ggsave(savepath, p_impact, width = 16, height = 15,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()
