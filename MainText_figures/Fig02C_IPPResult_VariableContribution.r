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
library(extrafont)

# library(devtools)
# install_github("GuidoAMoreira/bayesPO")
library(bayesPO)

setwd("F:/ade2541-manuscript/Database")

# List .rds files of IPP model output data
files = list.files(".", pattern = '^IPP_.*.rds$', full.names = T)
po_est_all = read_rds(files, refhook = NULL)
# cov_importance = po_est_all$covariates_importance
rm(files)
# 1 - Simple plot ##################################################################
# par(mfrow = c(1, 2))
# boxplot(po_est_all$covariates_importance[[1]]) # Intensity variables
# boxplot(po_est_all$covariates_importance[[2]]) # Observability variables
# par(mfrow = c(1, 1))

# 2 - Formated plot #################################################################
# 2.1 - Format the data.frames ######################################################
# Intensity Covariate Importance
df_intens = data.frame(po_est_all$covariates_importance[c('intensity')])
names(df_intens) = c('8-HAND', 
                     '1-Temp. warmest quarter',
                     '2-Precip. wettest quarter',
                     '3-Precip. driest quarter',
                     '5-Clay content',
                     '6-Silt content',
                     '7-TPI',
                     '4-SCC',
                     '9-Distance to river')
df_intens = melt(df_intens)
df_intens$vari = 'Intensity'

# Observability Covariate Importance
df_obs = data.frame(po_est_all$covariates_importance[c('observability')])
names(df_obs) = c('10-Distance to road',
                  '11-Tree cover')
df_obs = melt(df_obs)
df_obs$vari = 'Obs.'

# 2.2 - Merge Intensity & Observability data ######################################################
df_contrib = rbind(df_intens, df_obs)
rm(df_intens, df_obs, po_est_all)

# Sort plot order
df_contrib$o = factor(df_contrib$variable)
levels(df_contrib$o) = levels(factor(sapply(str_split(df_contrib$o, "-"), "[", 1)))
df_contrib$variable_numb = as.numeric(df_contrib$o) 
df_contrib$variable_name = sapply(str_split(df_contrib$variable, "-"), "[", 2)
df_contrib$variable_name = factor(df_contrib$variable_name)

# 2.3 ggplot #####################################################################################
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
        strip.text = element_text(size = 7, family = 'Arial Narrow', color = "black", 
                                  margin = margin(0, 0, 0, 1.5, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        plot.subtitle = element_text(size = 8, family = 'Arial Narrow', face = 'bold', margin = margin(2, 0, 1, 0, "pt")),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.text.x = element_text(size = 6, family = 'Arial Narrow', colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(1, 0, .5, 0, "pt")),
        axis.text.y = element_text(size = 6, family = 'Arial Narrow', colour = 'black', margin = margin(0, .5, 0, 0, "pt")),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

savepath = str_c("./rplots/IPPModel03_VariableContribution.pdf")
ggsave(savepath, p_varicontrib, width = 4.45, height = 3.85,  units = "cm", dpi = 400, 
       bg = 'transparent', device = cairo_pdf)
dev.off()
