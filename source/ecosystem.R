
# Load the raster package
library(raster)

# Je met le chemin pour mon GeoTIFF
file_path <- "/Users/thomasrota/Desktop/biodiversity data analyse/Worldecosystem/WorldEcosystem.tif"


# Je lance le raster GeoTIFF
ecosystem_raster <- raster(file_path)


Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" )

## crop and mask
r2 <- crop(ecosystem_raster, extent(Switzerland))
ecosystem_switzerland <- mask(r2, Switzerland)
#plot(ecosystem_switzerland)

spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
#plot01 <- plot(spatial_points,add=T,pch=16,cex=2)
#print(plot01)

# J'extrais des valeurs pour ajouter des variables. 
eco_values <- raster::extract(ecosystem_switzerland, spatial_points)

# Print the extracted values
print(values)


######### metadata: 

metadat_eco <- read.delim("/Users/thomasrota/Desktop/biodiversity data analyse/Worldecosystem/WorldEcosystem.metadata.tsv")

#je vérifie les noms des colonnes. 
#colnames(metadat_eco)
################################################

#Je viens associer ici de nouvelles variables pour ma matrix. Notamment "eco_values", "Moisture", "Landcover", "Landforms" etc.. 
matrix_full_eco <- matrix_full
matrix_full_eco$eco_values <- eco_values


#Je supprime tout les NA pour avoir une matrix propre.
matrix_full_eco <- matrix_full_eco[complete.cases(matrix_full_eco), ]


# J'associe les climats, à mes espèces. 
matrix_full_eco <- merge(matrix_full_eco, metadat_eco, by.x="eco_values", by.y= "Value",all.x =T)

plot_ecosystem <- plot(matrix_full_eco)
print(plot_ecosystem)
