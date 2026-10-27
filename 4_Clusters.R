#####CODE TO PERFORM A CLUSTER ANALYSIS TO THE FIRES RELATED TO AIR POLLUTION IN THE ABURRA VALLEY TO IDENTIFY
###### AREAS OF INTEREST

#import packages
{
  library(terra)
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(purrr)
}

#Set working directory
wd="path_to_working_directory"
setwd(wd)

#Folders
pathretro <- paste0(wd,"/Data")
pathrasters <- paste0(wd,"/Rasters")
pathshapes <- paste0(wd,"/Shapes")

dir(pathrasters)
#Import raster dataset
fires<- rast(paste0(pathrasters,"/frp_sum_20182024_10km_oct2125.tif"))
plot(fires)
fires

# FRP total NSA

#Transform to NA all the pixels with value 0
fires[fires == 0] <- NA  # Remove zeros

# Convert raster to data frame
NSA_fires <- as.data.frame(fires, xy = TRUE, na.rm = TRUE)
summary(NSA_fires)

#Stats
Q1 <- quantile(NSA_fires$sum, 0.25)
Q3 <- quantile(NSA_fires$sum, 0.75)
IQR_val <- Q3 - Q1

#Filter fires
NSAupper <- Q3 + 1.5 * IQR_val

summary(NSA_fires$sum)
quantile(NSA_fires$sum, probs = c(0.75,0.80,0.85,0.90,0.95, 0.99, 0.995))
NSAq99 <- quantile(NSA_fires$sum, 0.99)

NSA_fires <- NSA_fires[NSA_fires$sum <= NSAupper, ]

# #  Convert dataframe back to SpatRaster
NSA_fires_rast <- terra::rast(
  NSA_fires,
  type = "xyz",   # interpret columns as x, y, value
  crs = crs(fires)
)
# 
# #  Align to original raster grid (optional but recommended)
# # Ensures the new raster matches original resolution & extent
 NSA_fires_rast <- terra::resample(NSA_fires_rast, fires, method = "near")

#Import polygon
colombia <- vect(paste0("C:/Users/tur75984/OneDrive - Temple University/ResearchAssis/CODI/GIS-CODI/Shapes/Colombia.shp"))  # Must be in UTM 18N (EPSG:32618)
colombia <- terra::project(colombia,crs(fires))

#Mask or crop your FRP raster to Colombia
frp_col <- mask(fires, colombia)

#Remove background pixels

#Transform to NA all the pixels with value 0
#Those zeros are "non-fire" pixels and should not be included in clustering — 
#DBSCAN is density-based and would treat dense clusters of zeros as valid.

#fires[fires == 0] <- NA
frp_col[frp_col == 0] <- NA  # Remove zeros
plot(frp_col)

#Convert raster into a data frame and prepare the data

# Convert raster to data frame
df <- as.data.frame(frp_col, xy = TRUE, na.rm = TRUE)
summary(df)

# Let's do some basic exploration
hist(df[,3])

# Just coordinates for DBSCAN
coords <- df[, c("x", "y")]

#####Eliminar valores extremos de FRP

#Filtrar o recortar los valores de FRP que estén por encima del 99 percentil para 
#evitar que esos pocos puntos condicionen toda la clasificación.

#SOLO COLOMBIA
# q99 <- quantile(df$sum, 0.99)
# df_filtered <- df[df$sum <= q99, ]

#NSA
df_filtered <- NSA_fires

######## K MEANS
set.seed(123)
centers_list <- c(3, 4, 5, 6)

# Total FRP for full study area
total_frp_all <- sum(df_filtered$sum, na.rm = TRUE)

# Create a list to store both results and cluster assignments
cluster_results <- list()

for (k in centers_list) {
  km <- kmeans(df_filtered[, c("x", "y", "sum")], centers = k, iter.max = 150, nstart = 10)
  
  df_clustered <- df_filtered %>%
    mutate(k = k,
           cluster = km$cluster)
  
  # Store both the clustered data and the summary
  cluster_results[[as.character(k)]] <- list(
    data = df_clustered,
    stats = df_clustered %>%
      group_by(k, cluster) %>%
      summarise(
        n_points   = n(),
        mean_frp   = mean(sum, na.rm = TRUE),
        median_frp = median(sum, na.rm = TRUE),
        max_frp    = max(sum, na.rm = TRUE),
        sd_frp     = sd(sum, na.rm = TRUE),
        total_frp  = sum(sum, na.rm = TRUE),
        perc_frp   = 100 * total_frp / total_frp_all,
        centroid_x = mean(x, na.rm = TRUE),
        centroid_y = mean(y, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

# Combine all summary tables into one
cluster_stats_list <- map_df(cluster_results, "stats")
write.csv(cluster_stats_list,"clusters10km_Stats_dic5_2025.csv")

# Function to plot clusters for a given k
plot_kmeans <- function(k) {
  ggplot(cluster_results[[as.character(k)]]$data,
         aes(x = x, y = y, color = factor(cluster))) +
    geom_point(size = 0.3, alpha = 0.8) +
    # geom_point(
    #   data = cluster_results[[as.character(k)]]$stats,
    #   aes(x = centroid_x, y = centroid_y),
    #   color = "black", size = 2.5, shape = 4, stroke = 1
    # ) +
    # geom_text(
    #   data = cluster_results[[as.character(k)]]$stats,
    #   aes(x = centroid_x, y = centroid_y, label = cluster),
    #   color = "black", size = 3, vjust = -1
    # ) +
    geom_text(
      data = cluster_results[[as.character(k)]]$stats,
      aes(x = centroid_x, y = centroid_y,
          #label = paste0(cluster, " (", round(perc_frp, 1), "%)")),
          label = paste0(round(perc_frp, 1), "%")),
      color = "black", size = 3, vjust = -1
    )+
    coord_equal() +
    theme_minimal() +
    labs(
      title = paste("k =", k),
      color = "Cluster"
    ) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      legend.position = "none"
    )
}

# Generate plots for each k
plots <- map(c(3, 4, 5, 6), plot_kmeans)

# Combine them into a 2×2 grid
final_plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]])
final_plot
ggsave("FRP_10km_clustersNSA_dic5_2025.jpeg",dpi = 300)#,height=50,width=50,units = "cm")





#Rasterize the kmeans
set.seed(123)

kmeans_result <- kmeans(df_filtered, centers = 3, iter.max = 150, nstart = 10) 
df_filtered$cluster_k <- kmeans_result$cluster

df_clust <- df_filtered  # or optionally: df[df$cluster_k != 0, ]

sf_points <- st_as_sf(df_clust, coords = c("x", "y"), crs = 32618)
v_points <- vect(sf_points)

clustrast_kmeans <- fires[[1]]
values(clustrast_kmeans) <- NA

cell_indices <- cells(clustrast_kmeans, v_points)
clustrast_kmeans[cell_indices[,2]] <- df_clust$cluster_k

plot(clustrast_kmeans, main = "K-means Clusters (X, Y, FRP)")
#writeRaster(clustrast_kmeans, paste0(pathrasters,"/FRP10km_kmeans_filtered_clusters3_dic5_2025.tif"), overwrite = TRUE)

#Convert to sf object
df_sf <- st_as_sf(df_filtered, coords = c("x", "y"), crs = 32618)


# Group by cluster and create hulls
hulls <- df_sf %>%
  group_by(cluster_k) %>%
  summarise(geometry = st_convex_hull(st_union(geometry)), .groups = "drop")


plot(st_geometry(df_sf), col = "red", border = "grey80")
plot(st_geometry(hulls), border = "red", lwd = 2, add = TRUE)

#st_write(hulls, paste0(pathshapes,"/FRP10km_kmeans_filtered_convexhulls_k3_dic5_2025.shp"), delete_layer = TRUE)

#### summarize FRP stats per cluster
df_filtered %>%
  group_by(cluster_k) %>%
  summarise(
    n_points = n(),
    mean_frp = mean(sum, na.rm = TRUE),
    median_frp = median(sum, na.rm = TRUE),
    max_frp = max(sum, na.rm = TRUE),
    sd_frp = sd(sum, na.rm = TRUE)
  )


#### proporción del FRP total acumulado por clúster

# FRP total por cluster
frp_cluster <- df_filtered %>%
  group_by(cluster_k) %>%
  summarise(
    total_frp = sum(sum, na.rm = TRUE)
  )

# FRP total global
total_frp_all <- sum(df_filtered$sum, na.rm = TRUE)

# Calcular proporción
frp_cluster <- frp_cluster %>%
  mutate(
    proportion = total_frp / total_frp_all * 100  # en porcentaje
  )

frp_cluster

############### CLUSTERING INFORMATION FOR CLUSTERS WITH HIGHER FRP PROPORTION
#CLUSTERS 3 AND 4: NORTH COLOMBIA

# Merge clusters 1, 2 
mask_poly <- hulls %>%
  filter(cluster_k %in% c(1,2)) %>%
  st_union() %>%
  st_sf()  # Convert back to sf

#Mask the fires with the new combined polygon

# First, make sure your polygon and raster have the same CRS
hull_merged <- st_transform(mask_poly, crs(fires))

# Mask the raster using the polygon
frp_masked_34 <- mask(NSA_fires_rast, vect(hull_merged))
plot(frp_masked_34)

#Convert masked raster to points for reclustering

# Convert to points (optional step for clustering)
frp_pts <- as.points(frp_masked_34, na.rm = TRUE, values = TRUE)
#writeVector(frp_pts,paste0(pathshapes,"/Fires10kmcluster124.shp"))

frp_df <- as.data.frame(frp_masked_34, xy = TRUE,na.rm=TRUE)
names(frp_df)[3] <- "FRP"

# Filter out low or zero values if needed
frp_df <- frp_df[frp_df$FRP > 0, ]

###CLuster analysis
#SCale data
frp_scaled <- scale(frp_df[, c("x", "y", "FRP")])

################################################ Hierarchichal clusters
library(stats)
coords <- frp_df[, c("x", "y")]

# Compute distance matrix
dist_matrix <- dist(coords)

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "average")  # or "ward.D2", "complete", etc.

clusters_hc <- cutree(hc, h = 350000)  # or specify k = 5
frp_df$cluster_hc <- clusters_hc


# # Cut into clusters (e.g., 5 clusters)
# frp_df$cluster_hc <- cutree(hc, k = 5)
# plot(hc, labels=FALSE)


ggplot(frp_df, aes(x = x, y = y, color = as.factor(clusters_hc))) +
  geom_point(size = 1.2) +
  scale_color_viridis_d(option = "plasma", name = "Cluster") +
  theme_minimal() +
  labs(title = "Hierarchical Clustering of Fires (FRP + Spatial)")


#Convert to sf object
fire_areas <- st_as_sf(frp_df, coords = c("x", "y"), crs = 32618)

# Group by cluster and create hulls
hulls <- fire_areas %>%
  group_by(cluster_hc) %>%
  summarise(geometry = st_convex_hull(st_union(geometry)), .groups = "drop")

plot(st_geometry(hulls), border = "red", lwd = 2)#, add = TRUE

#st_write(hulls, paste0(pathshapes,"/areas_hc100k.shp"), delete_layer = TRUE)

# Crear objeto sf y aplicar concaveman por grupo
library(lwgeom)   # for st_concave_hull

# Optional: remove very tiny clusters before hulls
fire_areas_filt <- fire_areas %>%
  group_by(cluster_hc) %>%
  filter(n() >= 5) %>%    # e.g. keep clusters with ≥5 points
  ungroup()

fire_hulls <- fire_areas_filt %>%
  group_by(cluster_hc) %>%
  summarise(
    total_frp = sum(FRP, na.rm = TRUE),
    n_points  = n(),
    geometry  = st_concave_hull(
      st_union(geometry),  # merge all points in the cluster
      ratio = 0.2          # 0–1: smaller = more concave, 1 = convex hull
    ),
    .groups = "drop"
  ) %>%
  st_cast("MULTIPOLYGON")  # for clean polygon geometry

plot(st_geometry(fire_hulls), col = NA, border = "red")
plot(st_geometry(fire_areas), add = TRUE, pch = 16, cex = 0.3)

#Export
st_write(fire_hulls, paste0(pathshapes,"/fire_cluster_hulls_dic052025.shp"), delete_layer = TRUE)


#Rasterize the kmeans
fire_areas <- vect(fire_areas)

clustrast_kmeans <- frp_masked_34[[1]]
values(clustrast_kmeans) <- NA

cell_indices <- cells(clustrast_kmeans, fire_areas)
clustrast_kmeans[cell_indices[,2]] <- frp_df$cluster_hc

plot(clustrast_kmeans, main = "K-means Clusters (X, Y, FRP)")
#writeRaster(clustrast_kmeans, paste0(pathrasters,"/areas10km_hc350k_dic5_2025.tif"), overwrite = TRUE)


#### summarize FRP stats per cluster
x <- frp_df %>%
  group_by(cluster_hc) %>%
  summarise(
    n_points = n(),
    mean_frp = mean(FRP, na.rm = TRUE),
    median_frp = median(FRP, na.rm = TRUE),
    max_frp = max(FRP, na.rm = TRUE),
    sd_frp = sd(FRP, na.rm = TRUE)
  )
print(x,n=25)

#### proporción del FRP total acumulado por clúster

# FRP total por cluster
frp_cluster <- frp_df %>%
  group_by(cluster_hc) %>%
  summarise(
    total_frp = sum(FRP, na.rm = TRUE)
  )

# FRP total global
#total_frp_hc <- sum(frp_df$FRP, na.rm = TRUE)

# Calcular proporción relativa
frp_cluster <- frp_cluster %>%
  mutate(
    proportion = total_frp / total_frp_all * 100  # en porcentaje
  )

frp_cluster
#Join with cluster stats
frp_cluster <- cbind(x,frp_cluster[,2:3])

print(frp_cluster)

#Filter clusters
important_clusters <- frp_cluster %>%
  filter(proportion >= 0.1)   # keep >= 0.5%
important_clusters[,c(1,8)]
#Apply filter to the dataset
selected_ids <- important_clusters$cluster_hc

# filter your original fire points
fires_kept <- df_clustered %>%   # your df that contains cluster_hc column
  filter(cluster_hc %in% selected_ids)

write.csv(frp_cluster,"hc_cluster10km.csv")

# Verifica cuántos píxeles se usaron en cada tabla
n_kmeans <- sum(df_filtered$cluster_k %in% c(2, 3))
n_hierarchical <- sum(frp_df$cluster_hc %in% c(2, 3, 4, 6, 7))  # según como agrupaste zonas A y B

# Verifica suma total de FRP en cada grupo
sum_kmeans <- sum(df_filtered$sum[df_filtered$cluster_k %in% c(2, 3)])
sum_hc <- sum(frp_df$FRP[frp_df$cluster_hc %in% c(2, 3, 4, 6, 7)])

###Estadisticos por zona 

# Zona A: clusters 2 y 3
zona_a <- frp_df %>% 
  filter(cluster_hc %in% c(2, 3)) %>% 
  summarise(
    n_points = n(),
    mean_frp = mean(FRP),
    median_frp = median(FRP),
    max_frp = max(FRP),
    sd_frp = sd(FRP),
    total_frp = sum(FRP)
  )

# Zona B: clusters 4, 6 y 7
zona_b <- frp_df %>% 
  filter(cluster_hc %in% c(4, 6, 7)) %>% 
  summarise(
    n_points = n(),
    mean_frp = mean(FRP),
    median_frp = median(FRP),
    max_frp = max(FRP),
    sd_frp = sd(FRP),
    total_frp = sum(FRP)
  )

#%FRP
total_frp_all <- sum(frp_df$FRP)

zona_a <- zona_a %>% mutate(pct_frp = (total_frp / total_frp_all) * 100)
zona_b <- zona_b %>% mutate(pct_frp = (total_frp / total_frp_all) * 100)

################### FRP inside and outside of Colombia

fires_df <- as.data.frame(fires, xy = TRUE, na.rm = TRUE)

# Convert FRP dataframe to sf points
frp_points <- st_as_sf(NSA_fires, coords = c("x", "y"), crs = 32618) #q99
frp_points <- st_as_sf(fires_df, coords = c("x", "y"), crs = 32618)

#TRansform to sf from spatvector
colombia_sf <- st_as_sf(colombia)

# Make sure both objects use the same CRS
colombia_sf <- st_transform(colombia_sf, st_crs(frp_points))

# Step 3: Identify which points are inside Colombia
frp_points$inside_colombia <- st_within(frp_points, colombia_sf, sparse = FALSE)[,1]

# Step 4: Compute FRP totals
total_frp <- sum(frp_points$sum, na.rm = TRUE)
frp_inside <- sum(frp_points$sum[frp_points$inside_colombia], na.rm = TRUE)
frp_outside <- total_frp - frp_inside

# Step 5: Percentages
pct_inside <- 100 * frp_inside / total_frp
pct_outside <- 100 * frp_outside / total_frp

# Results
cat("FRP inside Colombia:", round(frp_inside, 2), "(", round(pct_inside, 1), "% )\n")
cat("FRP outside Colombia:", round(frp_outside, 2), "(", round(pct_outside, 1), "% )\n")

######################### 10 km spatial resolution raster

# Aggregate to 10 km (i.e. factor of 10 if resolution is 1 km)
# Use "sum" because we want total FRP per 10x10 km cell
fires_10km <- aggregate(fires, fact = 10, fun = sum, na.rm = TRUE)

# Plot the result
plot(fires_10km, main = "FRP Sum (10x10 km resolution)")
plot(fires)
#Export
writeRaster(fires_10km, paste0(pathrasters,"/frp_sum_10km.tif"), overwrite = TRUE)


# Summary statistics
# Extract values as a vector
vals <- values(fires_10km, na.rm = TRUE)

# Compute quantiles
quantiles <- quantile(vals, probs = c(0, 0.25, 0.5, 0.75,0.99, 1), na.rm = TRUE)
print(quantiles)


# Create a boxplot
boxplot(vals,
        main = "Boxplot of FRP (10km resolution)",
        ylab = "FRP",
        col = "orange",
        border = "darkred")