# Load necessary library
library(cluster)  # For clustering functions
library(clusterCrit)
library(dendextend)
library(ggplot2)
library(pheatmap)

set.seed(123)  # For reproducibility
# Prepare the data
d<-iris[,1:4]
data_matrix <- as.matrix(d)

pca_result <- prcomp(data_matrix, center = TRUE, scale = TRUE)
png("unit3/output/p3_hca_pca.png", width = 1000, height = 1200)
# Generate heatmap
heatmap(pca_result$x, scale = "row", col = heat.colors(10))
dev.off()
