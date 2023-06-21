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

# library(devtools)
# install_github("GuidoAMoreira/bayesPO")
library(bayesPO)

setwd("F:/ade2541-manuscript/Database")

# List .rds files of IPP model output data
files = list.files(".", pattern = '^IPP_.*.rds$', full.names = T)
po_est_all = read_rds(files, refhook = NULL)
rm(files)

# 1 - Response curves ###########################################################
# 1.1 - Format the data #########################################################
ranges <- po_est_all@backgroundSummary[c(1, 6), ]
colnames(ranges) <- gsub(" ", "", colnames(ranges), fixed = TRUE)
for (i in 1:length(ranges)) ranges[i] = as.numeric(gsub(" ", "", substr(ranges[i], 9, 17))); rm(i)
rownames(ranges) <- c("Min.", "Max.")
ranges <- ranges[, do.call(c, sapply(names(po_est_all$parameters), function(n) which(n == colnames(ranges)))) ]
Rcpp::cppFunction(
  '
List calcImpacts(NumericMatrix params, NumericMatrix ranges) {
  NumericMatrix tempMat(ranges.nrow(), 9);
  List output(params.nrow());
  for (int i = 0; i < params.nrow(); i++) {
    tempMat = clone(tempMat);
    for (int j = 0; j < ranges.nrow(); j++)
      for (int k = 0; k < 9; k++) {
        tempMat(j, k) = 1 - exp(-exp(
        -R::logspace_add(0, -params(i, 0) - params(i, k + 1) * ranges(j, k) )
        ) * params(i, 13) * 1e-6);
      }
    output[i] = tempMat;
  }
  return output;
}
'
)

n_r <- 100
all_ranges <- apply(ranges, 2, function(r) seq(r[1], r[2], len = n_r))
impacts <- calcImpacts(as.matrix(po_est_all), all_ranges)
rm(po_est_all, all_ranges, ranges)

#Intensity Rasters
#Range of minValue and max maxValue found on intensity rasters (check SM for public sources)
vari_ranges <- t(cbind(c(-1.000000, 3.570030e+03),   #HAND
                       c(0.300000, 3.000000e+01),    #Temp. warmest quarter
                       c(129.000000, 2.605000e+03),  #Precip. wettest quarter
                       c(5.000000, 1.021000e+03),    #Precip. driest quarter
                       c(90.405342, 7.698105e+02),   #Clay content
                       c(45.774564, 7.323059e+02),   #Silt content
                       c(-481.625000, 5.095000e+02), #TPI
                       c(-1.071907, 6.319447e-01),   #SCC
                       c(0.000000, 4.333141e+04)))   #Distance to river

# Name variables with formating pattern
vari_names = c("8-HAND~(m)", 
               "1-Temp.~warmest~quarter~(ºC)",
               "2-Precip.~wettest~quarter~(mm)",
               "3-Precip.~driest~quarter~(mm)",
               "5-Clay~content~(g~'/'~kg)",
               "6-Silt~content~(g~'/'~kg)",
               "7-TPI",
               "4-SCC~(log[10]~'[X]')",
               "9-Distance~to~river~(m)")


dfs <- lapply(1:9, function(j) {
  pd <- do.call(cbind, lapply(impacts, function(i) i[, j]))
  data.frame(
    variable = vari_names[j],
    val_x = seq(vari_ranges[j, 1], vari_ranges[j, 2], len = n_r),
    q25 = apply(pd, 1, quantile, 0.025),
    qm = rowMeans(pd),
    q975 = apply(pd, 1, quantile, 0.975)
  )
})
rm(impacts, n_r, vari_names, vari_ranges, calcImpacts)

df_response = rbindlist(dfs)
df_response = df_response[order(as.character(df_response$variable)),]
df_response = melt(df_response, id.vars = c('variable', 'val_x'))
names(df_response) = c('variable', 'val_x', 'quad', 'val_y')
rm(dfs)

# Sort plot order
df_response$o = factor(df_response$variable)
levels(df_response$o) = levels(factor(sapply(str_split(df_response$o, "-"), "[", 1)))
df_response$variable_numb = as.numeric(df_response$o) 

df_response$variable_name = sapply(str_split(df_response$variable, "-"), "[", 2)
df_response$variable_name = factor(df_response$variable_name)


# 1.2 ggplot #####################################################################################
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

options(scipen=999)
p_respcurves =
  ggplot(df_response, aes(x = val_x, y = val_y, color = quad))+
  geom_line(size=.5) +
  scale_colour_manual(values = c("#21918c", "#440154", "#fde725"), 
                      name = "Quartiles: ", labels = c("25%", "97.5%", "mean")) +
  scale_x_continuous(name='Response of presence probability to covariate data',
                     # labels=function(x) format(x, big.mark = ",", small.mark = ".", scientific = FALSE),
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
        strip.text = element_text(size = 7, family = 'Arial Narrow', color = "black",
                                  margin = margin(0, 0, 0, 0, "pt"), vjust = 0.5, hjust = 0.5),
        strip.background = element_rect(size=0.25, color = "black"),
        plot.subtitle = element_text(size = 8, family = 'Arial Narrow', face = 'bold', margin = margin(2, 0, 1, 0, "pt")),
        panel.grid.minor = element_line(size = 0.1),
        panel.grid.major = element_line(size = 0.2),
        panel.border = element_rect(fill = NA, size=0.25, color = "black"),
        axis.text.x = element_text(size = 6, family = 'Arial Narrow', colour = 'black', vjust = 0.5, hjust = 0.5, margin = margin(0, 0, 0, 0, "pt")),
        axis.text.y = element_text(size = 6, family = 'Arial Narrow', colour = 'black', margin = margin(0, 0, 0, 0, "pt")),
        axis.title.x = element_text(size =6.5, family = 'Arial Narrow', face = "bold", margin = margin(2, 0, 0, 0, "pt")),
        axis.title.y = element_text(size = 6.5, family = 'Arial Narrow', face = "bold", margin = margin(1, 0, 0, 0, "pt")),
        legend.title = element_blank(),
        legend.position = "none")

savepath = str_c("./rplots/IPPModel04_ResponseCurves.pdf")
ggsave(savepath, p_respcurves, width = 12, height = 5.45,  units = "cm", dpi = 400, 
       bg = 'white', device = cairo_pdf)
dev.off()
