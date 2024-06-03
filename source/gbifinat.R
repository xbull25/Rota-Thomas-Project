# L'objectif ici est de télécharger l'ensemble de mes données, et d'obtenir une matrice de base propre. 

library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(rinat)  
library(raster) 
library(sp)
library(ggfortify)
library(plotly)

# Je télécharge mes données d'occurences GBIF, en suisse pour Emys Orbicularis et Trachemys scripta. 

gbif_data1 <- occ_data(scientificName = "Emys orbicularis", country = "CH", hasCoordinate = TRUE, limit = 5000)

gbif_data2 <- occ_data(scientificName = "Trachemys scripta", country = "CH", hasCoordinate = TRUE, limit = 5000)


# J'extrais les occurrences GBIF pour la Suisse pour mes deux espèces. 
occur <- gbif_data1$data
gbif_data_switzerland1 <- occur[occur$country == "Switzerland", c("scientificName", "decimalLatitude", "decimalLongitude")] # nolint
#View(gbif_data_switzerland1)


occur <- gbif_data2$data
gbif_data_switzerland2 <- occur[occur$country == "Switzerland", c("scientificName", "decimalLatitude", "decimalLongitude")]
#View(gbif_data_switzerland2)

#Je combine les deux jeux de données, pour avoir sur la même matrice, les données GBIF de mes deux espèces. 

gbif_data_switzerland <- rbind(gbif_data_switzerland1, gbif_data_switzerland2)
#View(gbif_data_switzerland)


# Je récupère les données spatiales pour la Suisse
Switzerland <- ne_countries(scale = "medium", returnclass = "sf", country = "Switzerland")


# Plot des occurrences GBIF sur une carte de la Suisse
#ggplot(data = Switzerland) +
#  geom_sf() +
#  geom_point(data = gbif_data_switzerland, aes(x = decimalLongitude, y = decimalLatitude, color = scientificName), size = 4, 
      #       shape = 23) + 
#  scale_color_manual(values = c("Emys orbicularis" = "darkred", "Trachemys scripta" = "darkgreen"), 
                #     labels = c("Emys orbicularis", "Trachemys scripta")) +
#  theme_classic()


# J'extrais les données intéréssantes que je pourrais utilisé, issus de GBIF. Notamment pour les cartes. 
species <- gbif_data_switzerland$scientificName
latitude <- gbif_data_switzerland$decimalLatitude
longitude <- gbif_data_switzerland$decimalLongitude
source <- rep("gbif", length(species))

# Je créer une data frame pour les données GBIF. 
data_gbif <- data.frame(species, latitude, longitude, source)
#View(data_gbif)


## Je vais chercher les données sur INAT cette fois ci. Pour mes deux espèces. 

Emys_orb_inat <- get_inat_obs(query = "Emys orbicularis", place_id = "switzerland", maxresults = 500)
#View(Emys_orb_inat)

Tra_scri_inat <- get_inat_obs(query = "Trachemys scripta", place_id = "switzerland", maxresults = 500)
#View(Tra_scri_inat)

#Je combine les deux jeux de données inat de mes deux espèces. 
Emys_tra_inat <- rbind(Emys_orb_inat, Tra_scri_inat)
#View(Emys_tra_inat)


# Plot iNaturalist occurrences on a map of Switzerland
#ggplot(data = Switzerland) +
#  geom_sf() +
#  geom_point(data = Emys_tra_inat, aes(x = longitude, y = latitude), size = 4, 
#            shape = 23, fill = "darkred") + theme_classic()

# J'extrais encore une fois les données qui vont m'intérésser sur INAT. 

species <- Emys_tra_inat$scientific_name
latitude <- Emys_tra_inat$latitude
longitude <- Emys_tra_inat$longitude
source <- rep("inat", length(species))

# Je créer une data frame pour les données iNaturalist 
data_inat <- data.frame(species, latitude, longitude, source)
#View(data_inat)


#Maintenant je vais combiner mon jeu de donnée INAT et GBIF pour avoir une matrice globale.

matrix_full <- rbind(data_inat, data_gbif)
#colnames(matrix_full) #pour vérifier les colonnes)

# J'avais ici plusieurs noms de Trachemys et d'Emys (Des sous espèces). Je veux qu'elle s'apelle toute pareille pour simplifier les graphes par la suite. 
matrix_full$species[grep("Trachemys",matrix_full$species) ] <- "Trachemys scripta"
matrix_full$species[grep("Emys",matrix_full$species) ] <- "Emys orbicularis"

#View(matrix_full)

#Je créer un plot avec une carte, ou je vois la distribution de mes deux espèces. J'ai épuré la carte, avec un titre. 
library(ggplot2)


p <- ggplot(data = Switzerland) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 4, shape = 23) + 
  theme_minimal() +  # j'ai utilisé un thème pour que ce soit plus épuré, et la latitude et la longitute. 
  ggtitle("Distribution de Trachemys scripta et Emys orbicularis") +  
  labs(fill = "Espèces")  

print(p)

system('git config --global user.name "ROTA Thomas "')
system('git config --global user.email "rota.thomas@orange.fr"')
