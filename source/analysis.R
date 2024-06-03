# SUJET : L'objectif est de déterminer si Trachemys scripta et Emys orbicularis partagent les mêmes critères environnementaux pour se développer. 
#Si tel est le cas, il pourrait effectivement y avoir une compétition entre les deux espèces. 
#Actuellement, il existe très peu de recherches sur cette compétition. Nous allons pouvoir clarifier la situation.
#On va s'intérésser à l'altitude, à la température max et min, aux landscapes, aux NDVI, aux écosystèmes. 

####################################################################################################################################
####################################################################################################################################

# J'ai fais un boxplot de l'altitude en fonction des espèces. J'ai ajouté un titre, centré, et mis un titre à ma légende.

plot2 <- ggplot(data = matrix_full_eco_ele, aes(x = species, y = elevation, fill = species)) +
  geom_boxplot() +
  labs(x = "Species", y = "Altitude", fill = "Species", title = "Altitude by Species") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  guides(fill = guide_legend(title = "Species"))

print(plot2)

#Commentaire : On peut voir que les tendances d'altitudes sont relativement proche entre Emys et Trachemys. Ce boxplot 
#peut nous donner une idée générale, de ou ce situe les occurences en altitudes. Donc entre 100 et 600 mètres. 
#Quelques occurences sont très loins, et attention car ce n'est pas vraiment représentatif vue la marge d'erreur. 

####################################################################################################################################
####################################################################################################################################
#Je fais un graphique de la densité en fonction des précipitations annuelles moyenne pour mes deux espèces.

# J'ajoute a ma matrix clim uniquement les données que je veux (tmin et tmax)
matrix_clim$tmax_annuel <- matrix_full_eco_ele_clim_NDVI$tmax_annuel
matrix_clim$tmin_annuel <- matrix_clim$tmin_annuel 

# J'ajoute tous simplement les données de "espèce"
matrix_clim$species <- matrix_full_eco_ele_clim_NDVI$species

# Je fais un data frame, avec matrix_clim_long pour avoir l'ensemble de mes données sur le même. 
matrix_clim_long <- data.frame(
  species = rep(matrix_clim$species, 2),
  temperature = c(matrix_clim$tmin_annuel, matrix_clim$tmax_annuel),
  type = rep(c("Température Min", "Température Max"), each = nrow(matrix_clim))
)

#Je créer mon plot de densité, en fonction des précipitations annuelles. 
ggplot(matrix_clim, aes(x = rain_annuel, fill = species)) +
  geom_density(alpha = 0.5, adjust = 3) +
  labs(title = "Densité des espèces en fonction des précipitations annuelles",
       x = "Précipitations annuelles",
       y = "Densité",
       fill = "Espèce") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

#Commentaire : Encore une fois, on voit que Emys orbicularis se développerait bien mieux avec 
#des précipitations élevées que Trachemys, cependant, on dirait que Trachemys scripta à des exigences moins élevées
#car elle peut se développer avec des très faibles, et des très hautes précipitations contrairement
#a Emys orbicularis qui a certes un optimum élevé (100), mais en dehors de ça ne peut plus. 

####################################################################################################################################
####################################################################################################################################
# Je fais 2 graphiques sur le même plot, comparant la densité de mes 2 espèces, en fonction des températures minimums et maximums anuuelles.

ggplot(matrix_clim_long, aes(x = temperature, fill = species)) +
  geom_density(alpha = 0.5, adjust = 3) +
  facet_wrap(~ type, scales = "free") +
  labs(title = "Densité des espèces en fonction des températures minimales et maximales annuelles",
       x = "Température annuelle",
       y = "Densité",
       fill = "Espèce") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

#Commentaire: Pour le graph des Temp min : On voit que au dessous de 4 degrés, c'est compliqué pour les deux espèces. Cependant
# On dirait Emys orbicularis se développent largement mieux dans les températures froides avec
#Des densitées plus élevés. Ce qui expliquerait pourquoi elle est présente dans les zones plus haute
#en altitude. Trachemys a besoin de plus de chaleur. 

# Pour le graph des températures maximales, on voit que les deux espèces se développent bien dans le même intervalle de chaleurs. 
# Entre 10 et 17 degrés. Cependant, il y a une plus forte densité pour Emys orbicularis. 

####################################################################################################################################
####################################################################################################################################

#Je réalise ensuite 2 PCOA, afin de visualiser les ismilarités ou non entre mes occurences. 

df <- matrix_full_eco_ele_clim_NDVI
df <- na.omit(df)
df_continous <- df[,colnames(df) %in% c("tmin_annuel","elevation","NDVI","rain_annuel","tmax_annuel")]
df_discrete <- df[,!(colnames(df) %in% c("tmin_annuel","elevation","NDVI","rain_annuel","tmax_annuel"))]

df_continous <- apply(df_continous,2,as.numeric)

pca_res <- prcomp(df_continous, scale. = TRUE)

#Je fais le plot 

pcoa1 <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()
print(pcoa1)

# On a bien 4 variables distinctes. En regardant les cercles des espèces, on voit que ces deux espèces sont relativement
#proches au niveau des variables environnementales. Elles ont une niche écologique proche. 

####################################################################################################################################
####################################################################################################################################
#Je fais une deuxième PCOA, interactif cette fois ci. et en 3D. 
# Convert dataframe columns to numeric
df_continous <- apply(df_continous, 2, as.numeric)
pca <- princomp(df_continous, scores=T, cor=T)

scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

loads <- pca$loadings
load_names <- rownames(loads)
scale.loads <- 5

# Je fais mon plot 3D. 
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
            name = load_names[k]) 
}
print(p)

#Commentaire : on a l'impression que c'est l'altitude qui est la variable la plus importante. 

####################################################################################################################################
####################################################################################################################################
#Je réalise un plot intéractif de la température minimales en fonction de l'altitude. Par rapport à mes deux espèces
#Mais avant ca, je regarde si ces deux variables sont significative via un test de corrélation. 

df <- matrix_full_eco_ele_clim_NDVI
df <- na.omit(df)  

#Je sépare les variables continues et discrètes. 
df_continous <- df[, colnames(df) %in% c("tmin_annuel", "elevation", "NDVI", "rain_annuel")]
df_discrete <- df[, !(colnames(df) %in% c("tmin_annuel", "elevation", "NDVI", "rain_annuel"))]

data_stat <- df_continous
cor.test(data_stat$tmin_annuel, data_stat$elevation)

#Mon p-value est significatif, je fais alors un premie graph simple des températures minimales annuelles en fonction de l'élévation. 
#J'ai modifié la taille des points et mis une transparence car certains points étaient superposés et j'ai mis un titre.

temp_elevation_plot <- ggplot(df_continous, aes(x = elevation, y = tmin_annuel, color = df_discrete$species)) +
  geom_point(size = 3, alpha = 0.5) + 
  ggtitle("Température en fonction de l'élévation") +
  theme_classic() +
  labs(x = "Élévation (m)", y = "Température (°C)")
# J'affiche le graphique Température vs Élévation
print(temp_elevation_plot)

#Je le rend interactif
library(plotly)
intercative_tempele <- ggplotly(temp_elevation_plot)
intercative_tempele

#Commentaire: On voit clairement que la majorité de mes espèces sont entre 100 et 900m d'altitude, à des 
#températures comprises entre 2 et 8 degrés. 

####################################################################################################################################
####################################################################################################################################

#Je réalise maintenant un arbre, pour voir si j'ai bien deux différentes espèces, avec deux clades bien distincts. 

library(vegan)
library(ape)

df <- matrix_full_eco_ele_clim_NDVI[,colnames(matrix_full_eco_ele_clim_NDVI) %in% c("tmin_annuel","elevation","NDVI","rain_annuel","tmax_annuel")]

#verifie que toute les datas sont considérées comme numérique
df <- apply(df,2,as.numeric)

#ici c'est la distance matrix. Je réalise mon arbre. 
dist_df <- vegdist(scale(df), method = "euclidian") # method="man" # is a bit better
hclust_df<- hclust(dist_df, method = "complete")
dendro_df <-  as.phylo(hclust_df)
plot(dendro_df)

#Il y a bien deux clades différents, il s'agit bien de deux espèces différentes. Chaque ligne correspond à une occurence. 
# Il y a 2 environnements différents, avec des "sous" environnements, surement des hybrides. 

####################################################################################################################################
####################################################################################################################################

#Je vérifie si mes espèces se superpose, ou sont bien distincts via un graphique circulaire. 
library(BiocManager)
library(ggtree)
library(randomcoloR)

matrix_full_eco_ele_clim_NDVI$ID <- c(1:nrow(matrix_full_eco_ele_clim_NDVI))

ID <- as.factor(matrix_full_eco_ele_clim_NDVI$ID)

#ici on s'intéresse aux espèces, mais ca peut être un environnement. 
target_factor  <- as.factor(matrix_full_eco_ele_clim_NDVI$species)
g2 <- split(ID, target_factor)
g2

tree_plot <- ggtree::groupOTU(dendro_df, g2,group_name = "grouper")

#ici on génère juste des couleurs. Le distinct color me permet de différencier aux max les couleurs entre les espèces. 

cols <- distinctColorPalette(length(unique(target_factor))) ## several color
cols <- cols[1:length(unique(target_factor))]

#Je fais le graph

circ <- ggtree(tree_plot, aes(color = grouper), size = 1,layout = "circular") + 
  geom_tiplab(size = 3) +
  scale_color_manual(values = cols) 
print(circ)

#Commentaire : les espèces sont totalement en overlap (se chevauche), on a donc 2 espèces très très proches. 

####################################################################################################################################
####################################################################################################################################
#Je réalise une Heatmap, Ca organise tous les échantillons par similarité. (Ici c'est une observation). Organisation de nos points. 
library(ggfortify)
library(ggcorrplot)
library(pheatmap)

df <- matrix_full_eco_ele_clim_NDVI
df <- na.omit(df)  # Remove rows with missing values

# Je sépare à nouveau les valeurs continues, et discrètes. 
df_continous <- df[, colnames(df) %in% c("tmin_annuel", "elevation", "NDVI", "rain_annuel")]
df_discrete <- df[, !(colnames(df) %in% c("tmin_annuel", "elevation", "NDVI", "rain_annuel"))]

# Je prépare les datas pour ma heatmap
data <- df_continous
row.names(data) <- c(1:nrow(data))

# Je génère ma heatmap
heatmap(scale(data))

#Commentaire : j'ai des points qui sont relativement variés, avec des compositions très différentes. Forte variabilité. 

####################################################################################################################################
####################################################################################################################################
#Je vais réaliser quelques analyses concernant notamments les écosystèmes ou sont présent mes deux espèces. 
#Je réalise un boxplot, avec la température max annuel moyenne en fonction des différents écosystèmes. 
library(emmeans)

data_stat <- matrix_full_eco_ele_clim_NDVI

#Je fais le graph
P_fact <- ggplot(data = data_stat, mapping = aes(x = Landcover, y = tmax_annuel, fill = Landcover))
P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

#Ici tous deviens intéractifs. On peut zoomer dessus etc. 
print(P_fact)

#Je le rend interactif 
print(P_fact)
library(plotly)
ggplotly(P_fact)

#Je vérifie simplement si c'est significatif avec une ANOVA. 
#Je fais un modèl linéaire
linear_model <- lm(tmax_annuel ~ Landcover, data = data_stat)
#Puis ANOVA
anova(linear_model)
# Les variables tmax annuel et Landcover sont bien significatif. 

#Commentaire : On voit qu'au niveau des températures max, les deux espèces sont présentes principalement dans les terres cultivés
# entre 12 et 15 degrés, au niveau des forêts, et au niveau des zones ou les Hommes sont présents. 

####################################################################################################################################
####################################################################################################################################

#Je réalise un graphique pour voir si les écosystèmes de mes deux espèces sont relativements proches. 

library(fmsb)
data_stat <- matrix_full_eco_ele_clim_NDVI

# Agréger les données par W_Ecosystm
aggregated_data_species <- aggregate(
  cbind(elevation, rain_annuel, tmax_annuel,NDVI) ~ species, 
  data = data_stat, 
  FUN = mean
)
#J'affiche les résultats
print(aggregated_data_species)

min1 <- c(0,0,-10,0)
max2 <- c(apply(aggregated_data_species[,2:5],2,max))
sp1 <- aggregated_data_species[1,2:5]
sp2 <- aggregated_data_species[2,2:5]
row_id <- c(1,2,aggregated_data_species$species)
 
aggregated_data_species <- rbind(min1,max2,sp1,sp2)
row.names(aggregated_data_species) <- row_id

radarchart(aggregated_data_species)

#Commentaire : On voit que les écosystèmes de mes deux espèces sont relativement proche. Que ce soit par
#rapport au NDVI, au précipitation annuelle, ou encore à la température max. 
#La seule différence est au niveau de l'altitude encore une fois. 

#CONCLUSION 

# En sommes, après avoir regardé l'ensemble des critères environnementaux (Tmax, Tmin, Altitude, Précipitation, ecosystèmes, NDVI)
#, on remarque que la niche écologique de mes deux espèces est relativement proche. La seule différence se trouve notamment 
#Au niveau de l'altitude. En effet, on dirait que Emys orbicularis se développe dans des altitudes plus importantes, avec une densité plus 
#Forte. trachemys quand a elle, est moins exigeante d'un point de vue de ces critères environnementaux. C'est d'ailleurs pour cela
# que c'est une espèce invasive. En regardant de manière générale, on peut donc en conclure que ces deux espèces peuvent vivre dans 
#le même habitat, se qui est un vrai problème pour Emys orbicularis, car Trachemys scripta se développent plus rapidement, et peut lui
#Faire une vrai compétition. 

#Limites : Avec l'étude de seulement deux espèces, mes analyses sont clairement limitées. D'autant plus que ces deux espèces
#Sont relativement proches d'un point de vue écologique. 