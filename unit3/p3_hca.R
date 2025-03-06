# Prepare the data
data_matrix <- matrix(c(0, 1, 3, 3, 5, 1, 0, 3, 3, 5, 3, 3, 0, 2, 5, 3, 3, 2, 0, 4, 5, 5, 5, 4, 0), ncol = 5, byrow = TRUE)
# d <- matrix(c(1,2,3, 4,5,6, 7,8,9), ncol = 3, byrow = TRUE)
#d<-read.table("unit3/data/clusterdemo.csv", sep = ",", header = TRUE)
# d<-iris[,3:4]
# print(d)

# set labels for rows
# rownames(d) <- c("A", "B", "C", "D", "E") # Set meaningful labels

# print(d)
# dist_matrix <- dist(d, method = "euclidean")
# Convert to matrix format and print
# distance_matrix <- as.matrix(dist_matrix)
# convert to matrix format
# distance_matrix <- as.dist(d)
distance_matrix <- dist(data_matrix, method = "euclidean")

# Perform hierarchical clustering using single linkage
hc <- hclust(distance_matrix, method = "single") # Change "single" to "complete", "average", etc.

# Plot the dendrogram
png("unit3/output/p3_hca.png")
#plot(hc, labels = rownames(d), main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "Data Points")
plot(as.dendrogram(hc), main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "Data Points")
dev.off()

# print(distance_matrix)
