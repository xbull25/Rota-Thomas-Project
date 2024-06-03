#install.packages("sf")
library(sf)
#install.packages("elevatr")
library(elevatr)
#install.packages("raster")
library(raster)
library(ggplot2)
library(rayshader)
library(png)
library(magick)

sf_use_s2(FALSE)

# Je créer une carte de la Suisse. 

Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" )
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)
#plot(elevation_switzerland)

## crop and mask
r2 <- crop(elevation_switzerland, extent(Switzerland))
elevation_switzerland <- mask(r2, Switzerland)
#plot(elevation_switzerland)

#Ici, c'est pour mes cartes 3D. 
elmat <- raster_to_matrix(elevation_switzerland)
elevation.texture.map <- readPNG("/Users/thomasrota/Desktop/biodiversity data analyse/test/Switzerland2.png")
attr(elmat, "extent") <- extent(elevation_switzerland)
#J'ajoute mes points d'élévations sur ma matrix_full

latitude <- matrix_full_eco$latitude
longitude <- matrix_full_eco$longitude

# Create a data frame for GBIF data
gbif_coord <- data.frame(longitude,latitude)

# J'extrais mes points d'élévation. 
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation_switzerland, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

matrix_full_eco$elevation <- elevation_points
matrix_full_eco_ele <- matrix_full_eco
#View(matrix_full_eco_ele)

#Je convertis en numériques mes données d'altitudes. 

matrix_full_eco_ele$elevation <- as.numeric(matrix_full_eco_ele$elevation)

# J'ai fais un boxplot de l'altitude en fonction des espèces. J'ai ajouté un titre, centré, et mis un titre à ma légende

library(ggplot2)

plot2 <- ggplot(data = matrix_full_eco_ele, aes(x = species, y = elevation, fill = species)) +
  geom_boxplot() +
  labs(x = "Species", y = "Altitude", fill = "Species", title = "Altitude by Species") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  guides(fill = guide_legend(title = "Species"))

print(plot2)


