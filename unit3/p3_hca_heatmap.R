# Load necessary library
library(cluster)  # For clustering functions
library(clusterCrit)
library(dendextend)
library(ggplot2)
library(pheatmap)

set.seed(123)  # For reproducibility
# Prepare the data
d<-iris[,1:4]
d <- read.csv("unit3/data/clusterdemo.csv", header = TRUE)
data_matrix <- as.matrix(d)
# Generate heatmap
png("unit3/output/p3_hca_heatmap.png", width = 1000, height = 1200)
heatmap(data_matrix, scale = "row", col = heat.colors(10))
dev.off()
