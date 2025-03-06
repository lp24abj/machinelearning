# Load data
d <- expand.grid(x = 1:10, y = 1:10)
d$x <- 2 * d$x  # Scale x-values

png("unit3/output/p2_prcomp_simpledata.png",width = 1000, height = 1000)
par(mfrow = c(2, 2))
plot(d, main = "Original Data")
# Apply PCA
pca_result <- prcomp(d, center = TRUE, scale = TRUE)
pc_df <- as.data.frame(pca_result$x)

# Print variance explained
#how much each principal component contributes to the total variance.
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
print("Shows proportion of variance explained")
print(variance_explained)  # Shows proportion of variance explained
print("Shows the actual value of variance explained(codevalue):")
print(pca_result$sdev^2) # Shows the actual value of variance explained

# Scree plot to visualize 
plot(pc_df, main = "Projection onto PC1 and PC2")
plot(variance_explained, type = "b", main = "Scree Plot", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained")
dev.off()
png("unit3/output/p2_prcomp_simpledata_pairwise.png",width = 1000, height = 1000)
pairs(pc_df[, 1:2], main = "Pairwise Scatterplots of Principal Components")
dev.off()