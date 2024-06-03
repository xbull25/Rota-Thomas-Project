
library(ggfortify)
df <- matrix_full_eco_ele_clim_NDVI
df <- na.omit(df)
df_continous <- df[,colnames(df) %in% c("tmin_annuel","elevation","NDVI","rain_annuel","tmax_annuel")]
df_discrete <- df[,!(colnames(df) %in% c("tmin_annuel","elevation","NDVI","rain_annuel","tmax_annuel"))]

df_continous <- apply(df_continous,2,as.numeric)

pca_res <- prcomp(df_continous, scale. = TRUE)

#autoplot(pca_res)

##########################################################################################


##########################################################################################
##########################################################################################
#PCOA


##########################################################################################

p10 <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()
print(p10)
##########################################################################################
##########################################
##########################################
#Ici, je réalise une PCOA. 

row.names(df_continous) <- c(1:nrow(df_continous))
library(vegan)

dist_matt <- vegdist(df_continous, method = "euclidian")  #
D3_data_dist <- cmdscale(dist_matt, k = 3)
D3_data_dist <- data.frame(D3_data_dist)


cols <- df_discrete$species

#J'ai aggrandi les points pour une meilleure visibilitée
PCOA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = cols)) +
  geom_point(size = 3) + ggtitle("my project") +
  theme_classic()
print(PCOA)


# Graphique de la température minimale annuel en fonction de l'élévation. J'ai modifié la taille des points et mis une transparence car certains points étaient superposés et j'ai mis un titre
temp_elevation_plot <- ggplot(df_continous, aes(x = elevation, y = tmin_annuel, color = df_discrete$species)) +
  geom_point(size = 3, alpha = 0.5) + 
  ggtitle("Température en fonction de l'élévation") +
  theme_classic() +
  labs(x = "Élévation (m)", y = "Température (°C)")
# J'affiche le graphique Température vs Élévation
print(temp_elevation_plot)

##########################################################################################
##########################################################################################
library(plotly)
##interactive pour la PCOA

intercative_pcao <- ggplotly(PCOA)
intercative_pcao

#interactive pour température en fonction de l'altitude.
intercative_tempele <- ggplotly(temp_elevation_plot)
intercative_tempele


##########################################################################################
##########################################################################################
#### 3D

# Convert dataframe columns to numeric
df_continous <- apply(df_continous, 2, as.numeric)

# Perform PCA

# PCA
pca <- princomp(df_continous, scores=T, cor=T)

# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pca$loadings

# Loadings names
load_names <- rownames(loads)

# Scale factor for loadings
scale.loads <- 5

# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discrete$species)

for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}
print(p)