rm(list = ls(globalenv()))
gc()
options(encoding = "UTF-8") 
library(stringr)
library(readr)
library(raster)
library(corrplot)
library(RColorBrewer)
library(stats)

setwd("F:/ade2541-manuscript/Database/")

## 1.1 Bioclimatic - Pearson Correl Test #########################################################
# Check SM for public sources of raster data
files1 = list.files("./Rasters/WorldClim/", pattern = ".tif$", full.names = T)
files4 = list.files("./Rasters/OpenStreetMap/", pattern = ".tif$", full.names = T)
files5 = list.files("./Rasters/GFC/", pattern = ".tif$", full.names = T)

# Stack bioclimatic rasters & observability
r_bio = stack(files1, files4, files5); rm(files1, files4, files5)

# Pearson Correl Test
pearson_bio = layerStats(r_bio, 'pearson', na.rm=T)
cmatrix_bio = pearson_bio$'pearson correlation coefficient'
rm(pearson_bio, r_bio)
## 1.2 Bioclimatic - Correl Plot #########################################################
c.matrix = cor(cmatrix_bio)

varis.names.y = c("Bio.1: Annual mean temperature",
                  "Bio.2: Mean diurnal range",
                  "Bio.3: Isothermality",
                  "Bio.4: Temperature seasonality",
                  "Bio.5: Max. temperature of warmest month",
                  "Bio.6: Min. temperature of coldest month",
                  "Bio.7: Temperature annual range",
                  "Bio.8: Mean temperature of wettest quarter",
                  "Bio.9: Mean temperature of driest quarter",
                  "Bio.10: Mean temperature of warmest quarter",
                  "Bio.11: Mean temperature of coldest quarter",
                  "Bio.12: Annual precipitation",
                  "Bio.13: Precipitation of wettest month",
                  "Bio.14: Precipitation of driest month",
                  "Bio.15: Precipitation seasonality",
                  "Bio.16: Precipitation of wettest quarter",
                  "Bio.17: Precipitation of driest quarter",
                  "Bio.18: Precipitation of warmest quarter",
                  "Bio.19: Precipitation of coldest quarter",
                  "Distance to road",
                  "Tree cover")

varis.names.x = c("Bio.1",
                  "Bio.2",
                  "Bio.3",
                  "Bio.4",
                  "Bio.5",
                  "Bio.6",
                  "Bio.7",
                  "Bio.8",
                  "Bio.9",
                  "Bio.10",
                  "Bio.11",
                  "Bio.12",
                  "Bio.13",
                  "Bio.14",
                  "Bio.15",
                  "Bio.16",
                  "Bio.17",
                  "Bio.18",
                  "Bio.19",
                  "Dist. to road",
                  "Tree cover")
rownames(c.matrix) = varis.names.y
colnames(c.matrix) = varis.names.x
rm(varis.names.y, varis.names.x)

savepath = str_c("./rplots/sm/FigS16_CMatrixBio.png")
title = str_c("Correlation matrix - Bioclimatic data")
corr = T

png(savepath, height = 29, width = 40, units = 'cm', res=500, pointsize = 12)
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", 
         method="color", col=brewer.pal(n=10, name="PuOr"), #col = col(100), #definicao da cor!
         addgrid.col="white", 
         addCoef.col = NULL, is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = NA,  
         cl.pos = "b", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c",
         mar = c(0,0,0,0))
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", add = T, 
         method="number", col=c("red", rep("gray10", 8) ,"red"), 
         pch.cex = "transparent", bg = "transparent",
         addgrid.col='transparent', 
         addCoef.col = 'transparent', is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = 0.8, number.digits = 2, 
         cl.pos = "n", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c")
dev.off()
rm(savepath, title, corr, c.matrix)

## 2.1 Edaphic - Pearson Correl Test #########################################################
# Check SM for public sources of raster data
files1 = list.files("./Rasters/ZUQUIM/", pattern = ".tif$", full.names = T)
files2 = list.files("./Rasters/SoilGrids/", pattern = ".tif$", full.names = T)
files4 = list.files("./Rasters/OpenStreetMap/", pattern = ".tif$", full.names = T)
files5 = list.files("./Rasters/GFC/", pattern = ".tif$", full.names = T)

# Stack edaphic rasters & observability
r1 = stack(files1); r2 = stack(files2); r3 = stack(files4, files5)
r_edap = stack(r1, r2, r3); rm(files1, files2, files4, files5, r1, r2, r3)

# Pearson Correl Test
pearson_edap = layerStats(r_edap, 'pearson', na.rm=T)
cmatrix_edap = pearson_edap$'pearson correlation coefficient'
rm(pearson_edap, r_edap)

## 2.1 Edaphic - Correl Plot #########################################################
c.matrix = cor(cmatrix_edap)

varis.names.y = c("Soil cation concentration",
                  "Bulk density",
                  "Cation exchange capacity",
                  "Clay content",
                  "Coarse fragments",
                  "Organic carbon density",
                  "Organic carbon stock",
                  "pH water",
                  "Sand content",
                  "Silt content",
                  "Soil organic carbon",
                  "Total nitrogen",
                  "Distance to road",
                  "Tree cover")

varis.names.x = c("SCC",
                  "Bulk dens.",
                  "Cation exch. cap.",
                  "Clay content",
                  "Coarse fragments",
                  "Org. carbon dens.",
                  "Org. carbon stock",
                  "pH water",
                  "Sand content",
                  "Silt content",
                  "Soil org. carbon",
                  "Total nitrogen",
                  "Dist. to road",
                  "Tree cover")
rownames(c.matrix) = varis.names.y
colnames(c.matrix) = varis.names.x
rm(varis.names.y, varis.names.x)

savepath = str_c("./rplots/sm/FigS17_CMatrixEdap.png")
title = str_c("Correlation matrix - Edaphic data")
corr = T
png(savepath, height = 26, width = 28, units = 'cm', res=500, pointsize = 12)
# par(mar = c(0, 0, 0, 0) + 0.1)
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", 
         method="color", col=brewer.pal(n=10, name="PuOr"), #col = col(100), #definicao da cor!
         addgrid.col="white", 
         addCoef.col = NULL, is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = NA,  
         cl.pos = "b", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c")
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", add = T, 
         method="number", col=c("red", rep("gray10", 8) ,"red"), 
         pch.cex = "transparent", bg = "transparent",
         addgrid.col='transparent', 
         addCoef.col = 'transparent', is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = 0.8, number.digits = 2, 
         cl.pos = "n", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c")
dev.off()
rm(savepath, title, corr, c.matrix)


## 3.1 Topographic - Pearson Correl Test #########################################################
# Check SM for public sources of raster data
files1 = list.files("./Rasters/HydroSHEDS/", pattern = ".tif$", full.names = T)
files2 = list.files("./Rasters/AMBDATA/", pattern = ".tif$", full.names = T)
files3 = list.files("./Rasters/JRC/", pattern = ".tif$", full.names = T)
files4 = list.files("./Rasters/OpenStreetMap/", pattern = "Amz_DistRoads1km.tif$", full.names = T)
files5 = list.files("./Rasters/GFC/", pattern = "Amz_TreeCover20181km.tif$", full.names = T)

# Stack Topographic rasters & observability
r1 = stack(files1); r2 = stack(files2); r3 = stack(files3, files4, files5)
r_topo = stack(r1, r2, r3); rm(files1, files2, files3, files4, files5, r1, r2, r3)

# Pearson Correl Test
pearson_topo = layerStats(r_topo, 'pearson', na.rm=T)
cmatrix_topo = pearson_topo$'pearson correlation coefficient'
rm(pearson_topo, r_topo)

## 3.2 Topographic - Correl Plot #########################################################
c.matrix = cor(cmatrix_topo)

varis.names.y = c("Aspect",
                  "Elevation",
                  "Roughness",
                  "Slope",
                  "Topographic Position Index",
                  "Terrain Ruggedness Index",
                  "Height Above Nearest Drainage",
                  "Water accessibility",
                  "Distance to nearest river",
                  "Distance to nearest road",
                  "Tree cover")

varis.names.x = c("Aspect",
                  "Elevation",
                  "Roughness",
                  "Slope",
                  "TPI",
                  "TRI",
                  "HAND",
                  "Water access.",
                  "Dist. to river",
                  "Dist. to road",
                  "Tree cover")
rownames(c.matrix) = varis.names.y
colnames(c.matrix) = varis.names.x
rm(varis.names.y, varis.names.x)

savepath = str_c("./rplots/sm/FigS18_CMatrixTopo.png")
title = str_c("Correlation matrix - Topographic data")
corr = T
png(savepath, height = 21, width = 28, units = 'cm', res=500, pointsize = 12)
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", 
         method="color", col=brewer.pal(n=10, name="PuOr"), #col = col(100), #definicao da cor!
         addgrid.col="white", 
         addCoef.col = NULL, is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = NA,  
         cl.pos = "b", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c",
         mar = c(0,0,0,0))
corrplot(c.matrix, title="", type = "lower", diag = T, na.label = "NA", add = T, 
         method="number", col=c("red", rep("gray10", 8) ,"red"), 
         pch.cex = "transparent", bg = "transparent",
         addgrid.col='transparent', 
         addCoef.col = 'transparent', is.corr = corr, #order = "alphabet",
         tl.cex=1.0, tl.col="black", tl.srt=45,
         number.cex = 0.8, number.digits = 2, 
         cl.pos = "n", cl.cex=1.0, cl.ratio = 0.08, cl.align = "c")
dev.off()
rm(savepath, title, corr, c.matrix)
