#Ici, je réalise l'ensemble des mes maps.

#Je fais la map de la distribution de mes espèces en Suisse. 

p <- ggplot(data = Switzerland) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 4, shape = 23) + 
  theme_minimal() +  # j'ai utilisé un thème pour que ce soit plus épuré, et la latitude et la longitute. 
  ggtitle("Distribution de Trachemys scripta et Emys orbicularis en Suisse") +  
  labs(fill = "Espèces")  

print(p)

#Commentaire : On a ici la carte de la Suisse, avec la distribution de mes deux espèces. On remarque que leurs distributions
#est similaire, et les endroits d'abondance sont les mêmes. 

####################################################################################################################################
####################################################################################################################################

#Ici, je réalise une carte pour avoir la distribution de mes deux espèces, sur une carte ou on voit l'altitude. 

# Je convertis mes rasters en data frame. 
elevation_df <- as.data.frame(as(elevation_switzerland, "SpatialPixelsDataFrame"))
colnames(elevation_df) <- c("elevation", "x", "y")

# Je convertis ça en 1 seul objet pour que ce soit plus simple pour le graphique. 
data_stat_sf_alt <- st_as_sf(matrix_full_eco_ele_clim_NDVI, coords = c("longitude", "latitude"), crs = 4326)

# Je créer la carte avec ma distribution d'espèce, en fonction de l'altitude. 
alt_graph <- ggplot() +
  geom_tile(data = elevation_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(option = "viridis") +
  geom_sf(data = Switzerland, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = data_stat_sf_alt, aes(color = species), size = 2, alpha = 0.7) +
  labs(title = "Distribution des espèces en Suisse en fonction de l'altitude",
       fill = "Altitude",
       color = "Espèce") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
print(alt_graph)
#Commentaire : On voit très bien que la distribution de mes deux espèces se fait sur 
# des altitudes relativement basses. En tout cas pas au dessus de 1000 mètres. 

####################################################################################################################################
####################################################################################################################################
#Je fais maintenant une carte 3D ou l'on peut voir la distrubution de Emys orbicularis avec les valeurs de GBIF. 


#Ici, j'extrais uniquement les données de Emys orbicularis (Données GBIF). 
latitude <- gbif_data_switzerland1$decimalLatitude
longitude <- gbif_data_switzerland1$decimalLongitude

# Je créer une data frame avec les variables longitude, latitude. 
gbif_inat_coord <- data.frame(longitude,latitude)

# J'extrait les valeurs d'altitudes pour Emys. 
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_inat_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation_switzerland, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

# Je fais ma carte 3D qui ouvre ici une fenêtre 3D. 
elmat %>% 
  sphere_shade(texture = "bw") %>%
  add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
   add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
   add_water(detect_water(elmat), color = "lightblue") %>%
plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

# Je rajoute les points sur la map. 
render_points(
  extent = extent(Switzerland), size = 10,
  lat = gbif_inat_coord$latitude, long = gbif_inat_coord$longitude,
  altitude = elevation_points + 100, zscale = 150, color = "darkred"
)

####################################################################################################################################
####################################################################################################################################

#Je fais maintenant une carte 3D ou l'on peut voir la distrubution de Trachemys scripta avec les valeurs de GBIF. 

#Ici, j'extrais uniquement les données de Trachemys scripta (Données GBIF). 

latitude2 <- gbif_data_switzerland2$decimalLatitude
longitude2 <- gbif_data_switzerland2$decimalLongitude

# Je créer une data frame avec les variables longitude, latitude. 
gbif_inat_coord2 <- data.frame(longitude2,latitude2)

# J'extrait les valeurs d'altitudes pour Trachemys scripta 
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_inat_coord2, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation_switzerland, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

# Je fais ma carte 3D qui ouvre ici une fenêtre 3D. 
elmat %>% 
  sphere_shade(texture = "bw") %>%
  add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
   add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
   add_water(detect_water(elmat), color = "lightblue") %>%
plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

# Je rajoute les points sur la map. 
render_points(
  extent = extent(Switzerland), size = 10,
  lat = gbif_inat_coord2$latitude, long = gbif_inat_coord2$longitude,
  altitude = elevation_points + 100, zscale = 150, color = "#d3b20c"
)

####################################################################################################################################
####################################################################################################################################

#Commentaire général sur les maps : 

#Avec les premières maps, on voit très bien la distribution des espèces sur la Suisse. Elles ont une distribution similaire
#avec une présence sur des altitudes relativement faibles (Entre 250 et 700m). A propos des cartes 3D, cela nous permet 
#de mieux visiualiser la topographie de la Suisse, et donc l'emplacement des occurences. 