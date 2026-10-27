#### CODE to ESTIMATE FOREST LOSS

#Load packages
{
  library(terra)
  library(sf)
}

#Paths

#Path to AMVA shape
pathshapes='/data/home/TU/tur75984/FRP_forest/Shapes'#'Y:/FRP_forest/Shapes'

#Path to export rasters
pathraster='/data/home/TU/tur75984/FRP_forest/Rasters'#'Y:/FRP_forest/Rasters'

###########Raster statistics################
#Import training data
clusters=st_read(paste0(pathshapes,"/Areas_Col_jan282026.shp"))#"/areas_c5_14_10_dic052025.shp")) 
clusters_v=vect(clusters)

# ## Import rasters 2002, 2017 and 2023

r_forest17_m <- rast(paste0(pathraster, "/rforest_2017_reclass.tif"))
r_forest23_m <- rast(paste0(pathraster, "/rforest_2023_reclass.tif"))
r_forest02_m <- rast(paste0(pathraster, "/rforest_2002_reclass.tif"))

# Reclassify clusters
clusters_v=project(clusters_v, r_forest02_m)

#check point
print("Data imported")

#Mask rasters
r_forest17_m <- mask(crop(r_forest17_m, clusters_v), clusters_v)
#Export raster
writeRaster(r_forest17_m,paste0(pathraster, "/rforest_2017_reclassMask.tif"))
print("masked forest 2017")

r_forest23_m <- mask(crop(r_forest23_m, clusters_v), clusters_v)
#Export raster
writeRaster(r_forest23_m,paste0(pathraster, "/rforest_2023_reclassMask.tif"))
print("masked forest 2023")

r_forest02_m <- mask(crop(r_forest02_m, clusters_v), clusters_v)
#Export raster
writeRaster(r_forest02_m,paste0(pathraster, "/rforest_2002_reclassMask.tif"))
print("masked forest 2002")

################## DEFORESTATION: LAND-COVER CHANGE DETECTION #########

#Forest loss: pixels that were 1 in one year and 0 in the next one

#### 2002 to 2017
print("Land/cover change analysisi for 2002 and 2017")

# Initialize raster
trans_0217 <- rast(r_forest02_m)
values(trans_0217) <- NA
names(trans_0217) <- "transition_0217"

print("Create new change classes")
# Assign transitions
# Forest loss: 1 = loss, 0 = no loss
trans_0217[r_forest02_m == 1 & r_forest17_m == 1] <- 1  # stable forest
trans_0217[r_forest02_m == 0 & r_forest17_m == 0] <- 2  # stable non-forest
trans_0217[r_forest02_m == 1 & r_forest17_m == 0] <- 3  # forest loss
trans_0217[r_forest02_m == 0 & r_forest17_m == 1] <- 4  # forest gain
print("New transitions assigned")

#Add class labels
levels(trans_0217) <- data.frame(
  ID    = c(1, 2, 3, 4),
  class = c(
    "no_change_forest",
    "no_change_nonforest",
    "forest_loss",
    "forest_gain"
  )
)

#Export raster
writeRaster(trans_0217, paste0(pathraster, "/change_2002_2017.tif"),  overwrite = TRUE)
print("Exported change raster for 2002 and 2017")

#Count pixels per category per cluster

# Extract transition values by cluster
ex <- terra::extract(trans_0217, clusters_v)
print("Pixel values extracted")

# Remove NA (if any)
ex <- ex[!is.na(ex[,2]), ]

# Tabulate
tab <- with(
  ex,
  table(cluster_id = clusters_v$cluster_hc[ID], transition = ex[,2])
)

# Convert to data.frame
tab_df <- as.data.frame.matrix(tab)

# Ensure all 4 classes exist (even if zero)
for (i in 1:4) {
  if (!as.character(i) %in% colnames(tab_df)) {
    tab_df[, as.character(i)] <- 0
  }
}

# Rename columns nicely
colnames(tab_df) <- c(
  "px_no_change_forest",
  "px_no_change_nonforest",
  "px_forest_loss",
  "px_forest_gain"
)

tab_df$cluster_id <- rownames(tab_df)
rownames(tab_df) <- NULL
print("Table created: transition pixels by cluster")

#Add area in km2
cell_m2  <- prod(res(trans_0217))   # 27 * 27 = 729
cell_km2 <- cell_m2 / 1e6

area_df <- tab_df
area_df[, 1:4] <- area_df[, 1:4] * cell_km2

colnames(area_df)[1:4] <- paste0(colnames(area_df)[1:4], "_km2")
print("Table created: transition areas by cluster")

#Export tables
write.csv(tab_df,  "transition_2002_2017_pixels_by_cluster.csv", row.names = FALSE)
write.csv(area_df, "transition_2002_2017_area_km2_by_cluster.csv", row.names = FALSE)
print("Tables saved")


#Cleaning
rm(ex)
rm(tab)
rm(tab_df)
rm(area_df)

#### 2017 to 2023
print("Land/cover change analysisi for 2017 and 2023")

# Initialize raster
trans_1723 <- rast(r_forest17_m)
values(trans_1723) <- NA
names(trans_1723) <- "transition_1723"

print("Create new change classes")
# Assign transitions
# Forest loss: 1 = loss, 0 = no loss
trans_1723[r_forest17_m == 1 & r_forest23_m == 1] <- 1  # stable forest
trans_1723[r_forest17_m == 0 & r_forest23_m == 0] <- 2  # stable non-forest
trans_1723[r_forest17_m == 1 & r_forest23_m == 0] <- 3  # forest loss
trans_1723[r_forest17_m == 0 & r_forest23_m == 1] <- 4  # forest gain
print("New transitions assigned")

#Add class labels
levels(trans_1723) <- data.frame(
  ID    = c(1, 2, 3, 4),
  class = c(
    "no_change_forest",
    "no_change_nonforest",
    "forest_loss",
    "forest_gain"
  )
)

#Export raster
writeRaster(trans_1723, paste0(pathraster, "/change_2017_2023.tif"),  overwrite = TRUE)
print("Exported change raster for 2017 and 2023")

#Count pixels per category per cluster

# Extract transition values by cluster
ex <- terra::extract(trans_1723, clusters_v)
print("Pixel values extracted")

# Remove NA (if any)
ex <- ex[!is.na(ex[,2]), ]

# Tabulate
tab <- with(
  ex,
  table(cluster_id = clusters_v$cluster_hc[ID], transition = ex[,2])
)

# Convert to data.frame
tab_df <- as.data.frame.matrix(tab)

# Ensure all 4 classes exist (even if zero)
for (i in 1:4) {
  if (!as.character(i) %in% colnames(tab_df)) {
    tab_df[, as.character(i)] <- 0
  }
}

# Rename columns nicely
colnames(tab_df) <- c(
  "px_no_change_forest",
  "px_no_change_nonforest",
  "px_forest_loss",
  "px_forest_gain"
)

tab_df$cluster_id <- rownames(tab_df)
rownames(tab_df) <- NULL
print("Table created: transition pixels by cluster")

#Add area in km2
cell_m2  <- prod(res(trans_1723))   # 27 * 27 = 729
cell_km2 <- cell_m2 / 1e6

area_df <- tab_df
area_df[, 1:4] <- area_df[, 1:4] * cell_km2

colnames(area_df)[1:4] <- paste0(colnames(area_df)[1:4], "_km2")
print("Table created: transition areas by cluster")

#Export tables
write.csv(tab_df,  "transition_2017_2023_pixels_by_cluster.csv", row.names = FALSE)
write.csv(area_df, "transition_2017_2023_area_km2_by_cluster.csv", row.names = FALSE)
print("Tables saved")






