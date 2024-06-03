#install.packages("geodata")
library(geodata)

#Cadre de données : longitude/latitude
spatial_points <- SpatialPoints(coords = matrix_full_eco_ele[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

# Je récupère les données minimales de la suisse A partir de WorldClim. Avec une matrice ou chaque colonne représente 1 mois. 

sw_clim <- worldclim_country("switzerland", var = "tmin", path = tempdir())
sw_clim_br <- brick(sw_clim)

matrix_clim = NULL #créer deux matrices vides
vec_colnames = NULL #pour le nom des colonnes
#Aller chercher tout les mois et les extraire dans une matrix. 

for (i in 1:12)
{ 

raster_month <- sw_clim_br[[i]]
vec_colnames <- c(vec_colnames,names(raster_month)) #objet vide et on colle le nom du raster


raster_month <- extract(raster_month, spatial_points, method = 'bilinear')
matrix_clim <- cbind(matrix_clim,raster_month)
}

colnames(matrix_clim) <- vec_colnames

#permet d'avoir les 12 mois et la moyenne annuelle. 

tmin_annuel <- as.vector(rowMeans(matrix_clim)) #ici c'est seulement la moyenne annuel)

matrix_clim <- data.frame(matrix_clim,tmin_annuel)

#View(vec_mean_an)
#View(matrix_clim)


###################################################################
###################################################################
# Je récupère les données de précipitations pour la suisse cette fois ci. Avec le même système de boucle. 

sw_clim_pec <- worldclim_country("switzerland", var = "prec", path = tempdir())
sw_clim_pec <- brick(sw_clim_pec)

matrix_rain = NULL
rain_annuel = NULL

for (i in 1:12)
{
raster_month_rain <- sw_clim_pec[[i]]
rain_annuel <- c(rain_annuel, names(raster_month_rain))

raster_month_rain <- extract(raster_month_rain, spatial_points, method = 'bilinear')
matrix_rain <- cbind(matrix_rain, raster_month_rain)
}

colnames(matrix_rain) <- rain_annuel

rain_annuel <- as.vector(rowMeans(matrix_rain))
matrix_rain <- data.frame(matrix_rain, rain_annuel)


# Retrieving maximal temperature data for Switzerland
sw_clim2 <- worldclim_country("switzerland", var = "tmax", path = tempdir())
sw_clim_br2 <- brick(sw_clim2)

matrix_temp2 = NULL
tmax_annuel = NULL

for (i in 1:12) {
  raster_month_temp2 <- sw_clim_br2[[i]]
  tmax_annuel <- c(tmax_annuel, names(raster_month_temp2))
  
  raster_month_temp2 <- extract(raster_month_temp2, spatial_points, method = 'bilinear')
  matrix_temp2 <- cbind(matrix_temp2, raster_month_temp2)
}

colnames(matrix_temp2) <- tmax_annuel

tmax_annuel <- as.vector(rowMeans(matrix_temp2))
matrix_temp2 <- data.frame(matrix_temp2, tmax_annuel)

################# J'ajoute les données précipitations et températures minimales à ma matrix
matrix_full_eco_ele_clim <- data.frame(matrix_full_eco_ele,rain_annuel,tmin_annuel,tmax_annuel)
#View(matrix_full_eco_ele_clim)

# Load required library
# Load required library
library(ggplot2)

# Je fais un plot de des deux espèces en fonction de la moyenne des températures minimales annuelles et de la moyenne des précipitations annuels.

 plot1 <- ggplot(matrix_full_eco_ele_clim, aes(x = rain_annuel,y=tmin_annuel, color = species)) +
  geom_point() +
  theme_minimal()

print(plot1)
