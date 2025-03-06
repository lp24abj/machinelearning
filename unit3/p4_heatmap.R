# Prepare the data
d <- matrix(c(1,2,1,2,3,3,3,4,2,4,5,4), ncol = 3, byrow = TRUE)
# print(d)
d <- as.matrix(iris[,1:4])
data <- read.table("unit3/data/clusterdemo.csv", sep = ",", header = TRUE)
d <- as.matrix(data[,1:20])
set.seed(123)
# Plot the dendrogram

# Define the correlation dissimilarity function for the distance matrix
correlation_dissimilarity_matrix <- function(mat) {
  as.dist(1 - cor(t(mat), use = "pairwise.complete.obs", method = "pearson"))
}

png("unit3/output/p4_heatmap.png",width = 1000, height = 1200)
heatmap(d, scale = "row", col = heat.colors(10), distfun = correlation_dissimilarity_matrix)
dev.off()
