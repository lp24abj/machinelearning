# Load necessary library
library(cluster)  # For clustering functions
library(clusterCrit)
library(dendextend)
library(ggplot2)

set.seed(123)  # For reproducibility
# Prepare the data
d<-iris[,1:4]

distance_matrix <- dist(d, method = "euclidean")


# Perform hierarchical clustering using single linkage
hc <- hclust(distance_matrix, method = "single") # Change "single" to "complete", "average", etc.
# Cut tree into 2 clusters
clusters <- cutree(hc, k = 15)
dend <- as.dendrogram(hc)
# Compute Dunn's index
dunn_index <- intCriteria(as.matrix(d), clusters, "Dunn")
print(paste("Dunn Index:", dunn_index))

# # Step 3: Compute Cophenetic Distances
# cophenetic_dist <- cophenetic(hc)
# # Step 4: Compute Hubert’s Gamma (Correlation between distances)
# hubert_gamma <- cor(distance_matrix, cophenetic_dist, method = "pearson")
# # Print the result
# print(paste("Hubert's Gamma Index:", round(hubert_gamma, 4)))


# Plot the dendrogram
png("unit3/output/p3_hca_adv.png", width = 2000, height = 800)
#plot(hc, labels = rownames(d), main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "Data Points")
# plot(hc, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "Data Points")
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "Data Points")
rect.hclust(hc , k = 15, border = 2:6)
abline(h = 3, col = 'red')
dev.off()

# print(distance_matrix)
