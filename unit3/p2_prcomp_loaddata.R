# Load data
# Select columns 2 and 3
d <- data[, 1:2]
d <- iris[, 1:4]

png("unit3/output/p2_origin.png")
plot(d, main = "Original Data")
dev.off()
# Apply PCA
pca_result <- prcomp(d, center = TRUE, scale = TRUE)

# Print variance explained
#how much each principal component contributes to the total variance.
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
print("Shows proportion of variance explained")
print(variance_explained)  # Shows proportion of variance explained
print("Shows the actual value of variance explained(codevalue):")
print(pca_result$sdev^2) # Shows the actual value of variance explained
# Scree plot to visualize variance distribution
png("unit3/output/p2_screeplot.png")
plot(variance_explained, type = "b", main = "Scree Plot", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained")
dev.off()

# Convert PCA results to dataframe
pc_df <- as.data.frame(pca_result$x)


# Scatter plot of first two principal components
png("unit3/output/p2_pcafunc.png")
plot(pc_df, main = "Projection onto PC1 and PC2")
dev.off()

# Pairwise scatterplots of first two principal components
png("unit3/output/p2_pairwise.png")
pc_df$Species <- iris$Species
pairs(pc_df[, 1:4], col = as.numeric(pc_df$Species), main = "Pairwise Scatterplots of Principal Components")
dev.off()

