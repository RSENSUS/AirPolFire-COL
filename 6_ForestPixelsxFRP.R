##Code to estimate pixels of different land covers (forest, non/forest and deforested) 
# per FRP pixels.

#Load packages
{
  library(terra)
  library(sf)
}

#Paths

#Path to AMVA shape
pathshapes='/data/home/TU/tur75984/FRP_forest/Shapes'
#pathshapes='Y:/FRP_forest/Shapes'

#Path to export rasters
pathraster='/data/home/TU/tur75984/FRP_forest/Rasters'
#pathraster='Y:/FRP_forest/Rasters'

############### Import data
clusters=st_read(paste0(pathshapes,"/Areas_Col_jan282026.shp")) 
clusters_v=vect(clusters)

# ## Import rasters 

#Forest 2002, 2017 and 2023 
r_forest17_m <- rast(paste0(pathraster, "/rforest_2017_reclassMask.tif"))
r_forest23_m <- rast(paste0(pathraster, "/rforest_2023_reclassMask.tif"))
r_forest02_m <- rast(paste0(pathraster, "/rforest_2002_reclassMask.tif"))

#FRP sum
frp_i <- rast(paste0(pathraster, "/frp_sum_20182024_oct0125.tif"))#"/frp20182024_masked.tif"))

#Land-cover change from 2002 to 2017 and 2017 to 2023
change0217 <- rast(paste0(pathraster, "/change_2002_2017.tif"))
change1723 <- rast(paste0(pathraster, "/change_2017_2023.tif"))

#check point
print("Data imported")

# Reclassify clusters
clusters_v=project(clusters_v, r_forest23_m) #r_forest23

# #FRP pre-processing
# 
# # Convert zero or negative FRP values to NA
frp_i[frp_i <= 0] <- NA
#
# #Reclassify FRP raster
frp_i <- project(frp_i, crs(r_forest23_m), method="bilinear")
#
# # #Mask raster
frp <- mask(crop(frp_i, clusters_v), clusters_v)
#Checkpoint
print("masked FRP raster")

#Export pre-processed FRP raster
#writeRaster(frp,paste0(pathraster, "/frp20182024_masked.tif"))

### 2002 
### RESAMPLE FOREST DATA
# print("Resampling raster...")
# #Aggregate forest raster to 1 km FRP pixel 
# forest_frac_1km <- aggregate( r_forest02_m, 
#                               fact = round(res(frp)[1] / res(r_forest02_m)[1]),
#                               fun = mean, na.rm = TRUE ) 
# # Align exactly to FRP grid print("Aligning forest raster") 
# forest_frac_1km <- resample(forest_frac_1km, frp, method = "near")
# 
# #Convert fraction - binary forest/non-forest
# forest02_1km <- (forest_frac_1km >= 0.5) * 1
# names(forest02_1km) <- "forest_2002_1km"
# 
# #Mask
# forest02_1km  <- mask(forest02_1km, clusters_v)
# #Export resampled forest raster
# writeRaster(forest02_1km,paste0(pathraster, "/forest2002_resampled.tif"),overwrite=T)
# print("Resample done and raster exported")
# 
# #Build helper rasters: FRP on forest vs non-forest
# #FRP values outside that land cover become NA
# print("Calculating FRP values per land cover")
# frp_forest    <- frp * (forest02_1km == 1)
# frp_nonforest <- frp * (forest02_1km == 0)
# 
# #Extract per cluster
# 
# #FRP pixel counts
# print("Extracting pixel counts per land cover and cluster")
# px_forest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 1, na.rm = TRUE)
# )
# 
# px_nonforest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 0, na.rm = TRUE)
# )
# 
# #FRP sum
# print("Extracting FRP sum per land cover and cluster")
# frp_forest_sum <- terra::extract(
#   frp_forest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# frp_nonforest_sum <- terra::extract(
#   frp_nonforest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# #Total FRP per cluster
# frp_total_sum <- terra::extract(
#   frp,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# # Total number of 1 km pixels in each cluster
# px_total <- terra::extract(
#   frp,
#   clusters_v,
#   fun = function(x) sum(!is.na(x))
# )
# 
# 
# #Table
# out_frp <- data.frame(
#   cluster_id        = clusters_v$cluster_hc,
#   #Pixel counts
#   px_forest_1km     = px_forest[,2],
#   px_nonforest_1km  = px_nonforest[,2],
#   px_total          = px_total[,2],
#   #FRP sum
#   frp_forest_sum    = frp_forest_sum[,2],
#   frp_nonforest_sum = frp_nonforest_sum[,2],
#   frp_total_sum     = frp_total_sum[,2]
# )
# print("Done for 2002!")
# # #Export
# write.csv(out_frp, "forest_nonforest_by_cluster2002.csv", row.names = FALSE)
# 
# 
# #### 2017
# ### RESAMPLE FOREST DATA
# print("Resampling raster...")
# 
# #Aggregate forest raster to 1 km FRP pixel 
# forest_frac_1km <- aggregate( r_forest17_m, 
#                               fact = round(res(frp)[1] / res(r_forest17_m)[1]),
#                               fun = mean, na.rm = TRUE ) 
# 
# # Align exactly to FRP grid print("Aligning forest raster") 
# forest_frac_1km <- resample(forest_frac_1km, frp, method = "near")
# 
# #Convert fraction - binary forest/non-forest
# forest02_1km <- forest_frac_1km >= 0.5
# forest02_1km <- forest02_1km * 1
# names(forest02_1km) <- "forest_2017_1km"
# 
# #Mask
# forest02_1km  <- mask(forest02_1km, clusters_v)
# #Export resampled forest raster
# writeRaster(forest02_1km,paste0(pathraster, "/forest2017_resampled.tif"),overwrite=T)
# print("Resample done and raster exported")
# 
# #Build helper rasters: FRP on forest vs non-forest
# #FRP values outside that land cover become NA
# print("Calculating FRP values per land cover")
# frp_forest    <- frp * (forest02_1km == 1)
# frp_nonforest <- frp * (forest02_1km == 0)
# 
# #Extract per cluster
# 
# #FRP pixel counts
# print("Extracting pixel counts per land cover and cluster")
# px_forest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 1, na.rm = TRUE)
# )
# 
# px_nonforest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 0, na.rm = TRUE)
# )
# 
# #FRP sum
# print("Extracting FRP sum per land cover and cluster")
# frp_forest_sum <- terra::extract(
#   frp_forest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# frp_nonforest_sum <- terra::extract(
#   frp_nonforest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# #Total FRP per cluster
# frp_total_sum <- terra::extract(
#   frp,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# # Total number of 1 km pixels in each cluster
# px_total <- terra::extract(
#   frp,
#   clusters_v,
#   fun = function(x) sum(!is.na(x))
# )
# 
# 
# #Table
# out_frp <- data.frame(
#   cluster_id        = clusters_v$cluster_hc,
#   #Pixel counts
#   px_forest_1km     = px_forest[,2],
#   px_nonforest_1km  = px_nonforest[,2],
#   px_total          = px_total[,2],
#   #FRP sum
#   frp_forest_sum    = frp_forest_sum[,2],
#   frp_nonforest_sum = frp_nonforest_sum[,2],
#   frp_total_sum     = frp_total_sum[,2]
# )
# print("Done for 2017!")
# 
# #Export
# write.csv(out_frp, "forest_nonforest_by_cluster2017.csv", row.names = FALSE)
# 
# #### 2023
# ### RESAMPLE FOREST DATA
# print("Resampling raster...")
# 
# #Aggregate forest raster to 1 km FRP pixel 
# forest_frac_1km <- aggregate( r_forest23_m, 
#                               fact = round(res(frp)[1] / res(r_forest23_m)[1]),
#                               fun = mean, na.rm = TRUE ) 
# # Align exactly to FRP grid print("Aligning forest raster") 
# forest_frac_1km <- resample(forest_frac_1km, frp, method = "near")
# 
# #Convert fraction - binary forest/non-forest
# forest02_1km <- forest_frac_1km >= 0.5
# forest02_1km <- forest02_1km * 1
# names(forest02_1km) <- "forest_2023_1km"
# 
# #Mask
# forest02_1km  <- mask(forest02_1km, clusters_v)
# #Export resampled forest raster
# writeRaster(forest02_1km,paste0(pathraster, "/forest2023_resampled.tif"),overwrite=T)
# print("Resample done and raster exported")
# 
# #Build helper rasters: FRP on forest vs non-forest
# #FRP values outside that land cover become NA
# print("Calculating FRP values per land cover")
# frp_forest    <- frp * (forest02_1km == 1)
# frp_nonforest <- frp * (forest02_1km == 0)
# 
# #Extract per cluster
# 
# #FRP pixel counts
# print("Extracting pixel counts per land cover and cluster")
# px_forest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 1, na.rm = TRUE)
# )
# 
# px_nonforest <- terra::extract(
#   forest02_1km,
#   clusters_v,
#   fun = function(x) sum(x == 0, na.rm = TRUE)
# )
# 
# #FRP sum
# print("Extracting FRP sum per land cover and cluster")
# frp_forest_sum <- terra::extract(
#   frp_forest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# frp_nonforest_sum <- terra::extract(
#   frp_nonforest,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# #Total FRP per cluster
# frp_total_sum <- terra::extract(
#   frp,
#   clusters_v,
#   fun = sum,
#   na.rm = TRUE
# )
# 
# # Total number of 1 km pixels in each cluster
# px_total <- terra::extract(
#   frp,
#   clusters_v,
#   fun = function(x) sum(!is.na(x))
# )
# 
# 
# #Table
# out_frp <- data.frame(
#   cluster_id        = clusters_v$cluster_hc,
#   #Pixel counts
#   px_forest_1km     = px_forest[,2],
#   px_nonforest_1km  = px_nonforest[,2],
#   px_total          = px_total[,2],
#   #FRP sum
#   frp_forest_sum    = frp_forest_sum[,2],
#   frp_nonforest_sum = frp_nonforest_sum[,2],
#   frp_total_sum     = frp_total_sum[,2]
# )
# print("Done for 2023!")
# #Export
# write.csv(out_frp, "forest_nonforest_by_cluster2023.csv", row.names = FALSE)

############################## DEFORESTATION RASTERS ######################

#Robust majority function 
maj <- function(x, ...) {
  x <- as.integer(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Change 2002-2017
print("Resampling change 2002 to 2017 raster...")
# Majority (modal) class within each FRP pixel
# - ties="first" picks one class if there is a tie (rare but possible)

#store the raster values as integer type instead of factor/double
change0217 <- as.int(change0217)

#Factor for aggregation
fact <- round(res(frp) / res(change0217))
fact <- pmax(fact, 1)

#Resampling
trans_1km <- aggregate(change0217,fact=fact,
                       fun = maj, na.rm = TRUE, ties = "first")

# Align exactly to FRP grid (categorical -> near)
trans_1km <- resample(trans_1km, frp, method = "near")
names(trans_1km) <- "transition_0217_1km"

#Mask
trans_1km  <- mask(trans_1km, clusters_v)
#Export resampled forest raster
writeRaster(trans_1km,paste0(pathraster, "/change0217_resampled.tif"),overwrite=T)
print("Resample done and raster exported")

#Build helper rasters: FFRP resticted to each class
print("Calculating FRP values per land cover")

frp_c1 <- frp * (trans_1km == 1) #stable_forest 
frp_c2 <- frp * (trans_1km == 2) # stable non-forest
frp_c3 <- frp * (trans_1km == 3) # forest loss
frp_c4 <- frp * (trans_1km == 4) # forest gain

#Counts pixels per class per cluster
print("Extracting pixel counts per land cover and cluster")

px_c1 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 1, na.rm = TRUE))
px_c2 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 2, na.rm = TRUE))
px_c3 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 3, na.rm = TRUE))
px_c4 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 4, na.rm = TRUE))

px_total <- terra::extract(frp, clusters_v, fun = function(x) sum(!is.na(x)))

#Sum FRP per class per cluster
print("Extracting FRP sum per land cover and cluster")

frp_c1_sum <- terra::extract(frp_c1, clusters_v, fun = sum, na.rm = TRUE)
frp_c2_sum <- terra::extract(frp_c2, clusters_v, fun = sum, na.rm = TRUE)
frp_c3_sum <- terra::extract(frp_c3, clusters_v, fun = sum, na.rm = TRUE)
frp_c4_sum <- terra::extract(frp_c4, clusters_v, fun = sum, na.rm = TRUE)

frp_total_sum <- terra::extract(frp, clusters_v, fun = sum, na.rm = TRUE)

#Build the final table
out_frp_trans <- data.frame(
  cluster_id = clusters_v$cluster_hc,
  
  px_stable_forest     = px_c1[,2],
  px_stable_nonforest  = px_c2[,2],
  px_forest_loss       = px_c3[,2],
  px_forest_gain       = px_c4[,2],
  px_total_1km         = px_total[,2],
  
  frp_stable_forest    = frp_c1_sum[,2],
  frp_stable_nonforest = frp_c2_sum[,2],
  frp_forest_loss      = frp_c3_sum[,2],
  frp_forest_gain      = frp_c4_sum[,2],
  frp_total            = frp_total_sum[,2]
)


#Export 
write.csv(out_frp_trans, "FRP_by_transitionClass_2002_2017_by_cluster.csv", row.names = FALSE)
print("Done 2002 to 2017 analysis")


## Change 2017-2023
print("Resampling change 2017 to 2023 raster...")

#store the raster values as integer type instead of factor/double
change1723 <- as.int(change1723)

# Majority (modal) class within each FRP pixel
# - ties="first" picks one class if there is a tie (rare but possible)

#Factor for aggregation
fact <- round(res(frp) / res(change1723))
fact <- pmax(fact, 1)

#Resampling
trans_1km <- aggregate(change1723,fact=fact,
                       fun = maj, na.rm = TRUE, ties = "first")

# Align exactly to FRP grid (categorical -> near)
trans_1km <- resample(trans_1km, frp, method = "near")
names(trans_1km) <- "transition_1723_1km"

#Mask
trans_1km  <- mask(trans_1km, clusters_v)
#Export resampled forest raster
writeRaster(trans_1km,paste0(pathraster, "/change1723_resampled.tif"),overwrite=T)
print("Resample done and raster exported")

#Build helper rasters: FFRP resticted to each class
print("Calculating FRP values per land cover")

frp_c1 <- frp * (trans_1km == 1) #stable_forest 
frp_c2 <- frp * (trans_1km == 2) # stable non-forest
frp_c3 <- frp * (trans_1km == 3) # forest loss
frp_c4 <- frp * (trans_1km == 4) # forest gain

#Counts pixels per class per cluster
print("Extracting pixel counts per land cover and cluster")

px_c1 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 1, na.rm = TRUE))
px_c2 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 2, na.rm = TRUE))
px_c3 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 3, na.rm = TRUE))
px_c4 <- terra::extract(trans_1km, clusters_v, fun = function(x) sum(x == 4, na.rm = TRUE))

px_total <- terra::extract(frp, clusters_v, fun = function(x) sum(!is.na(x)))

#Sum FRP per class per cluster
print("Extracting FRP sum per land cover and cluster")

frp_c1_sum <- terra::extract(frp_c1, clusters_v, fun = sum, na.rm = TRUE)
frp_c2_sum <- terra::extract(frp_c2, clusters_v, fun = sum, na.rm = TRUE)
frp_c3_sum <- terra::extract(frp_c3, clusters_v, fun = sum, na.rm = TRUE)
frp_c4_sum <- terra::extract(frp_c4, clusters_v, fun = sum, na.rm = TRUE)

frp_total_sum <- terra::extract(frp, clusters_v, fun = sum, na.rm = TRUE)

#Build the final table
out_frp_trans <- data.frame(
  cluster_id = clusters_v$cluster_hc,
  
  px_stable_forest     = px_c1[,2],
  px_stable_nonforest  = px_c2[,2],
  px_forest_loss       = px_c3[,2],
  px_forest_gain       = px_c4[,2],
  px_total_1km         = px_total[,2],
  
  frp_stable_forest    = frp_c1_sum[,2],
  frp_stable_nonforest = frp_c2_sum[,2],
  frp_forest_loss      = frp_c3_sum[,2],
  frp_forest_gain      = frp_c4_sum[,2],
  frp_total            = frp_total_sum[,2]
)


#Export 
write.csv(out_frp_trans, "FRP_by_transitionClass_2017_2023_by_cluster.csv", row.names = FALSE)
print("Done 2017 to 2023 analysis")


