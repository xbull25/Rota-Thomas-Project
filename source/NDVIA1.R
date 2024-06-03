library(rgeoboundaries)
library(sf)
library(raster)
#install.packages("here")
library(here)
library(ggplot2)
library(viridis)


# Je télécharge les frontières de la Suisse
map_boundary <- geoboundaries("switzerland")

# Reading in the downloaded NDVI raster data
NDVI_raster <- raster("/Users/thomasrota/Desktop/biodiversity data analyse/Project/Data/modis/MYD13Q1_NDVI_2020_153.tif")

# Transforming the data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#plot(NDVI_raster)

# Cropping the data
map_boundary <- geoboundaries("switzerland")
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary))
#plot(NDVI_raster)

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(NDVI_raster) <- 0.0001

# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_eco_ele_clim[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
#plot(spatial_points,add=T)

####################################
####################################
#####################################

# Extract values
NDVI <- raster::extract(NDVI_raster, spatial_points)

###################
################# Je rajoute les données NDVI à ma matrix
matrix_full_eco_ele_clim_NDVI <- data.frame(matrix_full_eco_ele_clim,NDVI )
#View(matrix_full_eco_ele_clim_NDVI)


# JE fais juste un boxplot de controle. 
i <- ggplot(matrix_full_eco_ele_clim_NDVI, aes(x = species, y = NDVI, fill = species)) +
  geom_boxplot() +
  labs(title = "Distribution des valeurs de NDVI par espèce", x = "Espèce", y = "NDVI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none")

print(i)