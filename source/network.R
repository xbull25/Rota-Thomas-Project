######################################################################
######################################################################
############## networking

library(vegan)
library(ape)
#install.packages("igraph")
library(igraph)


df2 <- matrix_full_eco_ele_clim_NDVI[,colnames(matrix_full_eco_ele_clim_NDVI) %in% c("W_Ecosystm","tmin_annuel","tmax_annuel","elevation","NDVI","rain_annuel")]

aggregated_df2 <- aggregate(. ~ W_Ecosystm, data = df2 , FUN = mean)

df2_hclust <- aggregated_df2[,colnames(aggregated_df2) %in% c("tmin_annuel","tmax_annuel","elevation","NDVI","rain_annuel")]
#### scale between 0 and 1 
#define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

#scale values in 'sales' column to be between 0 and 1
df2_hclust_scaled <- apply(df2_hclust,2,scale_values)

row.names(df2_hclust_scaled) <- aggregated_df2$W_Ecosystm

dist_df2 <- vegdist(df2_hclust_scaled, method = "bray") # method="man" # is a bit better



g <- graph.adjacency(
  as.matrix(dist_df2),
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)


# Colour negative correlation edges as blue
E(g)[which(E(g)$weight<0.5)]$color <- "darkblue"

# Colour positive correlation edges as red
E(g)[which(E(g)$weight>0.5)]$color <- "darkred"


# Remove edges below absolute Pearson correlation 0.8
g <- delete_edges(g, E(g)[which(E(g)$weight<0.1)])

# Assign names to the graph vertices (optional)
V(g)$name <- V(g)$name

# Change shape of graph vertices
V(g)$shape <- "sphere"

# Change colour of graph vertices
V(g)$color <- "skyblue"

# Change colour of vertex frames
V(g)$vertex.frame.color <- "white"


# Amplify or decrease the width of the edges
edgeweights <- E(g)$weight * 2.0

# Convert the graph adjacency object into a minimum spanning tree based on Prim's algorithm
mst <- mst(g, algorithm="prim")

# Plot the tree object
networkplot <- plot(
  mst,
  layout=layout.fruchterman.reingold,
  edge.curved=TRUE,
  vertex.label.dist=-0.5,
  vertex.label.color="black",
  asp=FALSE,
  vertex.label.cex=0.6,
  edge.width=edgeweights,
  edge.arrow.mode=0,
  main="My first graph"
)
print(networkplot)