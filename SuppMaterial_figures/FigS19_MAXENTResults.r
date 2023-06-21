rm(list = ls(globalenv()))
gc()
removeTmpFiles(h=0)
dev.off()
options(encoding = "UTF-8") 
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
library(zoo)

setwd("F:/ade2541-manuscript/Database/Model_MaxEnt")
ifelse(!dir.exists(file.path('./', 'rplots')), dir.create(file.path('./', 'rplot')), FALSE)

# Check SM to replicate the MaxEnt model
csv = read.csv('./maxentResults.csv')

## Variable Individual contribution ========================================================================
df_contrib = str_detect(names(csv), pattern = '.contribution$', negate = FALSE)
df_contrib = csv[df_contrib]
df_contrib = df_contrib[1:1-nrow(df_contrib),]

names(df_contrib) = c('01. Mean temperature of warmest quarter',
                      '02. Precipitation of wettest quarter',
                      '03. Precipitation of driest quarter',
                      '04. Soil cation concentration',
                      '05. Clay content',
                      '06. Silt content',
                      '07. Topographic Position Index',
                      '08. Height Above Nearest Drainage', 
                      '09. Water distance')

df_contrib$vari = 'Intensity'
df_contrib = melt(df_contrib, id.vars = c('vari'))


df_contrib$o = factor(df_contrib$variable)
levels(df_contrib$o) = levels(factor(sapply(str_split(df_contrib$o, "\\. "), "[", 1)))
df_contrib$variable_numb = as.numeric(df_contrib$o) 

df_contrib$variable_name = sapply(str_split(df_contrib$variable, "\\. "), "[", 2)
df_contrib$variable_name = factor(df_contrib$variable_name)

df_contrib$variable_name = gsub("Mean temperature of warmest quarter",
                                "Mean temp. of warmest quarter", df_contrib$variable_name)
df_contrib$variable_name = gsub("Water distance",
                                "Distance to nearest river", df_contrib$variable_name)

p_varicontrib =
  ggplot(df_contrib, aes(x = value/100, y = reorder(variable_name, desc(variable_numb)))) +
    geom_boxplot(fill='#440154', width = .95, lty = 1, lwd = 0.05,
                 outlier.shape = 4, outlier.size = .5, outlier.alpha = 0.5,
                 color = '#21918c', alpha = 1) +
    stat_summary(fun=mean, geom="point", size=0.95, col='#fde725', shape=18) +
    scale_y_discrete(name="Variable") +
    scale_x_continuous(name = 'Relative contribution', oob = rescale_none, limits = c(0, 1), 
                       labels = percent,
                       breaks = seq(0, 1, by=.25),
                       expand = c(0,0)) + 
    facet_grid(vari~., scales = "free", space = "free") +
    labs(subtitle = 'Covariate contribution') +
    theme_bw() +
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

savepath_p4 = str_c("./rplots/FigS19C_VariableContribution.png")
ggsave(savepath_p4, p_varicontrib, width = 4.45, height = 3.85,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()

## Response curves ========================================================================
varis = str_detect(names(csv), pattern = '.contribution$', negate = FALSE)
varis = names(csv[varis])
varis = str_sub(varis, start = 2, end = -14)

df_final = NULL

for(i in 1:length(varis)) {
  vari = str_sub(varis[i])
  
  if (str_detect(varis[i], "Bio10")) {name = "01. Mean temperature of warmest quarter (?C)"}
  if (str_detect(varis[i], "Bio16")) {name = "02. Precipitation of wettest quarter (mm)"}
  if (str_detect(varis[i], "Bio17")) {name = "03. Precipitation of driest quarter (mm)"}
  if (str_detect(varis[i], "SoilCatConc")) {name = "04. Soil cation concentration (log10)"}
  if (str_detect(varis[i], "Clay")) {name = "05. Clay content (g/kg)"}
  if (str_detect(varis[i], "Silt")) {name = "06. Silt content (g/kg)"}
  if (str_detect(varis[i], "TPI")) {name = "07. Topographic Position Index"}
  if (str_detect(varis[i], "HAND")) {name = "08. Height Above Nearest Drainage (m)"}
  if (str_detect(varis[i], "DistRiver")) {name = "09. Water distance (m)"}
  
  pattern = str_c("^Earthwork", ".*\\_", vari, "_only.dat$")
  
  files = list.files("./plots", pattern = pattern, full.names = T, recursive = T)
  files = mixedsort(files)
  
  tmp = lapply(files, read.delim, sep=",", header = T)
  
  tmp = data.frame('vari' = name,
                   'x' = as.numeric(sapply(tmp, "[[", 2)),
                   'y' = as.numeric(sapply(tmp, "[[", 3)))
  
  if (str_detect(varis[i], "Clay") == T) {
    x = seq(min(tmp$x), max(tmp$x), len = 501)
    x = sort(x)
    
    df = data.frame('vari' = name,
                    'x' = x,
                    'y_mean' = NA,
                    'y_max' = NA,
                    'y_min' = NA)
    
    for (k in 1:length(x)) {
      ind = which(tmp$x == x[k])
      
      if(length(ind) == 0) {
        df[which(df$x == x[k]),][c(3,4,5)] = c(NA, NA, NA)}
      
      if(length(ind) != FALSE) {
        mean = mean(tmp[ind,]$y)
        max = max(tmp[ind,]$y)
        min = min(tmp[ind,]$y)
        df[which(df$x == x[k]),][c(3,4,5)] = c(mean, max, min)
        rm(ind, mean, max, min) }
    }
    
    df$y_mean = na.approx(df$y_mean)
    df$y_max = na.approx(df$y_max)
    df$y_min = na.approx(df$y_min)
    
  }
  
  if (str_detect(varis[i], "Clay") == F) {
    x = unique(tmp$x)
    
    df = data.frame('vari' = name,
                    'x' = x,
                    'y_mean' = NA,
                    'y_max' = NA,
                    'y_min' = NA)
    for (k in 1:length(x)) {
      ind = which(tmp$x == x[k])
      mean = mean(tmp[ind,]$y)
      max = max(tmp[ind,]$y)
      min = min(tmp[ind,]$y)
      df[which(df$x == x[k]),][c(3,4,5)] = c(mean, max, min)
      rm(ind, mean, max, min)}
  }
  
  df_final = rbind(df_final, df)
  rm(vari, pattern, files, tmp, df)
  
}; 
rm(i,k)


df_response = df_final
names(df_response)[1] = 'variable'

df_response$o = factor(df_response$variable)
levels(df_response$o) = levels(factor(sapply(str_split(df_response$o, "\\. "), "[", 1)))
df_response$variable_numb = as.numeric(df_response$o) 

df_response$variable_name = sapply(str_split(df_response$variable, "\\. "), "[", 2)
df_response$variable_name = factor(df_response$variable_name)


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
  mutate(variable_name = recode(variable_name,
                                "Mean temperature of warmest quarter (?C)" = "bold(Mean~temperature~of~warmest~quarter~(?C))",
                                "Precipitation of wettest quarter (mm)" = "bold(Precipitation~of~wettest~quarter~(mm))",
                                "Precipitation of driest quarter (mm)" = "bold(Precipitation~of~driest~quarter~(mm))",
                                "Soil cation concentration (log10)" = "bold(Soil~cation~concentration~(log[10]~'[X]'))",
                                "Clay content (g/kg)" = "bold(Clay~content~(g~'/'~kg))",
                                "Silt content (g/kg)" = "bold(Silt~content~(g~'/'~kg))",
                                "Topographic Position Index" = "bold(Topographic~Position~Index)",
                                "Height Above Nearest Drainage (m)" = "bold(Height~Above~Nearest~Drainage~(m))",
                                "Water distance (m)" = "bold(Distance~to~nearest~river~(m))"))

p_respcurves =
ggplot(data = df_response, aes(x=x, y=y_mean)) +
  # geom_line(size=.1, alpha=0.6, colour = viridis(1, begin = 1))
  geom_ribbon(aes(ymin=y_min, ymax=y_max), alpha=0.4, fill = viridis(1, begin = 0)) +
  geom_line(size=.5, alpha=1, colour = viridis(1, begin = 1)) +
  # stat_summary(geom="ribbon", fun.min="min", fun.max="max", alpha=0.6, fill = viridis(1, begin = 1))+
  # stat_summary(geom='line', fun = mean, color = viridis(1, begin = 0), size=1, alpha = 1) +
  scale_x_continuous(name='Response of presence probability to covariate data', expand = c(0,0)) +
  scale_y_continuous(name="Presence probability",
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


savepath_p5 = str_c("./rplots/FigS19D_ResponseCurves.png")
ggsave(savepath_p5, p_respcurves, width = 12, height = 5.45,  units = c("cm"), dpi = 400, bg = 'white')
dev.off()
