library(tiff)
library(raster)
library(rgdal)
library(dplyr)
library(bayesPO)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(Rcpp)

#### Reading data ####
## Background
# Intensity
height <- raster("02_Model_AmbVaris1km/Ambdata_HAND.tif")
temp <- raster("02_Model_AmbVaris1km/PresWC1_Bio10.tif")
precipWet <- raster("02_Model_AmbVaris1km/PresWC1_Bio16.tif")
precipDry <- raster("02_Model_AmbVaris1km/PresWC1_Bio17.tif")
clay <- raster("02_Model_AmbVaris1km/SoilGrid_Clay.tif")
silt <- raster("02_Model_AmbVaris1km/SoilGrid_Silt.tif")
TPI <- raster("02_Model_AmbVaris1km/TOPO_TPI.tif")
cation <- raster("02_Model_AmbVaris1km/ZuquimKrigeSoil_SoilCatConc.tif")
water <- raster("02_Model_AmbVaris1km/DistRiver_Amazonia1km.tif")
# Observability
roads <- raster("03_Model_ObsComp1km/DistRoads_Amazonia1km.tif")
tree_cover <- raster("03_Model_ObsComp1km/TreeCover2018_Amazonia1km.tif")

## The actual data
po_data <- read.csv("01_Earthworks_MergedDatasets.csv")
str(po_data)
names(po_data)
table(po_data$Relation_P.A)

#### Setting up the data for the model ####
background <- stack(
  height, temp, precipWet, precipDry, clay, silt, TPI, cation, roads, water, tree_cover
)

# PO data
background$po <- roads
background$po[which(!is.na(background$po[]))] <- 0
tab <- table(cellFromXY(background, po_data[, 1:2]))
background$po[as.numeric(names(tab))] <- tab; rm(tab)

names(background) <- c("height", "temp", "precipWet", "precipDry", "clay", "silt",
                       "TPI", "cation", "roads", "water", "tree_cover", "po")
background <- na.omit(cbind(xyFromCell(background$height,
                                       1:length(background$height)),
                            as.data.frame(background)))

background <- background %>%
  mutate(height2 = height^2, temp2 = temp^2, precipWet2 = precipWet^2,
         precipDry2 = precipDry^2, clay2 = clay^2, silt2 = silt^2, TPI2 = TPI^2,
         cation2 = cation^2, roads2 = roads^2, water2 = water^2, tree_cover2 = tree_cover^2,
         height_temp = height * temp, height_precipWet = height * precipWet,
         height_precipDry = height * precipDry, height_clay = height * clay,
         height_silt = height * silt, height_TPI = height * TPI,
         height_cation = height * cation, temp_precipWet = temp * precipWet,
         temp_precipDry = temp * precipDry, temp_clay = temp * clay,
         temp_silt = temp * silt, temp_TPI = temp * TPI,
         temp_cation = temp * cation, precipWet_precipDry = precipWet * precipDry,
         precipWet_clay = precipWet * clay, precipWet_silt = precipWet * silt,
         precipWet_TPI = precipWet * TPI, precipWet_cation = precipWet * cation,
         precipDry_clay = precipDry * clay, precipDry_silt = precipDry * silt,
         precipDry_TPI = precipDry * TPI, precipDry_cation = precipDry * cation,
         clay_silt = clay * silt, clay_TPI = clay * TPI, clay_cation = clay * cation,
         silt_TPI = silt * TPI, silt_cation = silt * cation, TPI_cation = TPI * cation,
         roads_water = roads * water, roads_tree_cover = roads * tree_cover,
         water_tree_cover = water * tree_cover)

means <- colMeans(background)
sds <- apply(background, 2, sd)

for (i in (1:ncol(background))[which(names(background) != "po")][- (1:2)])
  background[, i] <- (background[, i] - means[i]) / sds[i]
# colMeans(background)

#### Set up model ####
po_matrix <- background[background$po > 0, ]
for (i in 1:nrow(po_matrix))
  while (po_matrix$po[i] > 1){
    po_matrix$po[i] <- po_matrix$po[i] - 1
    po_matrix <- rbind(po_matrix, po_matrix[i, ])
    po_matrix$po[nrow(po_matrix)] <- 1
  }
po_matrix <- as.matrix(po_matrix)
background <- as.matrix(background)

library(doParallel)
library(foreach)
registerDoParallel(4)
inits <- list(
  initial(rep(0, 10), rep(0, 3), 1e-5),
  initial(rep(3, 10), rep(-3, 3), 1e-4),
  initial(rep(-3, 10), rep(3, 3), 1e-3),
  initial(-5:4, -1:1, 1e-2)
)
res <- foreach(i = 1:4) %dopar% { # Takes about a day
  po_model <- bayesPO_model(po = po_matrix, intensitySelection = 
                              c("height", "temp", "precipWet", "precipDry",
                                "clay", "silt", "TPI", "cation", "water"),
                            observabilitySelection = c("roads", "tree_cover"),
                            intensityLink = "logit", observabilityLink = "logit",
                            initial_values = inits[[i]],
                            joint_prior = prior(
                              NormalPrior(rep(0, 10), 10 * diag(10)),
                              NormalPrior(rep(0, 3), 10 * diag(3)),
                              GammaPrior(1e-4, 1e-4)
                            ))
  
  po_est <- fit_bayesPO(po_model, background, area = nrow(background) * 1e-6,
                        mcmc_setup = list(burnin = 2e5, thin = 20, iter = 2e5))
  saveRDS(po_est, paste0("Aux/est_0.4_multi",i,".rds"))
}
po_est_all <- do.call(c, lapply(
  1:4, function(i) readRDS(paste0("Aux/est_0.4_multi",i,".rds"))
))

plot(po_est_all$covariates_importance)


library(bayesplot)
output_array <- as.array(po_est_all)
color_scheme_set(scheme = "green")
mcmc_trace(output_array)
mcmc_dens(output_array)
summary(po_est_all)

#### Heat map ####
df.output <- as.data.frame(po_est_all)
obsInt <- which(names(df.output) == "Observability_Intercept")
bkgrnCols <- do.call(c, sapply(names(df.output)[1:obsInt], function(n) which(names(background) == n)))
logLS <- log(df.output$lambdaStar)
heatMap <- apply(background[bkgrnCols], 1, function(cell) {
  lp <- as.numeric(df.output$Intensity_Intercept) + cell %*% t(as.matrix(df.output[2:(obsInt - 1)]))
  mean(
    exp(-log1p(exp(-lp)) + logLS)
  ) * po_est_all$area * 1e-6
})
saveRDS(heatMap, "Aux/heatMap.rds")

