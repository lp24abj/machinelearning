# Load required package
if (!require("kohonen")) install.packages("kohonen", dependencies = TRUE)
library(kohonen)

# Load the dataset
# We'll use the built-in iris dataset for demonstration
# Normalize the data (excluding the Species column)
iris_data <- scale(iris[, -5])

# Set up the grid for SOM
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# Train the SOM
set.seed(123)  # For reproducibility
som_model <- som(iris_data, grid = som_grid, rlen = 100)
png("unit2/output/selfOrganizal.png")
# Plot the SOM results
par(mfrow = c(2, 2))

# Plot changes during training
plot(som_model, type = "changes", main = "Training Progress")

# Plot the map of node counts
plot(som_model, type = "counts", main = "Node Counts")

# Plot the mapping of samples
plot(som_model, type = "mapping", labels = as.integer(iris$Species), main = "Sample Mapping")

# Plot the codebook vectors
plot(som_model, type = "codes", main = "Codebook Vectors")

# Cluster analysis
# Perform hierarchical clustering on the SOM codes
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), k = 3)

# Plot the SOM with clusters
plot(som_model, type = "mapping", bgcol = rainbow(3)[som_cluster], main = "Clustered SOM")

# Add a legend for the clusters
add.cluster.boundaries(som_model, som_cluster)

cat("SOM implementation and visualization complete.")
dev.off()