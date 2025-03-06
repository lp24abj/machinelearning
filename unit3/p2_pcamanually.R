library(ggplot2)

pca <- function(data, n_components = NULL) {
    # test
    print(summary(data))
    print(sprintf("standard deviation x: %f", sd(data$x)))
    print(sprintf("standard deviation x: %f", sd(data$y)))

    # Step 1: Standardize the Data
    data_scaled <- scale(d, center = TRUE, scale = TRUE)
    # plot(data_scaled, col = "red", pch = 16, cex = 2, )
    # Step 2: Compute the Covariance Matrix
    covariance_matrix <- cov(data_scaled)
    print(covariance_matrix)
    # Step 3: Compute the Eigenvectors and Eigenvalues
    eig <- eigen(covariance_matrix)
    eigenvalues <- eig$values
    eigenvectors <- eig$vectors
    print("eigenvalues :")
    print(eigenvalues)
    print("eigenvectors :")
    print(eigenvectors)
    # Step 4: Sort Eigenvalues and Corresponding Eigenvectors
    sorted_indices <- order(eigenvalues, decreasing = TRUE)
    eigenvalues <- eigenvalues[sorted_indices]
    eigenvectors <- eigenvectors[, sorted_indices]

    # Step 5: Select Number of Principal Components
    if (is.null(n_components)) {
        n_components <- ncol(data)
    }
    eigenvectors <- eigenvectors[, 1:n_components]

    # Step 6: Transform the Data
    pca_transformed <- data_scaled %*% eigenvectors
    return(list(
        transformed_data = pca_transformed,
        eigenvalues = eigenvalues,
        eigenvectors = eigenvectors,
        data_scaled = data_scaled
    ))
}
# Step 1: Load the Data
# d <- expand.grid(x = 1:10, y = 1:10)
# d$x <- 2 * d$x
d <- iris[, 1:4]

# png("unit3/output/p2_pcamanual.png")
# par(mfrow = c(2, 2))
png("unit3/output/p2_manual_origin.png")
plot(d, main = "Original Data")
dev.off()
# Apply PCA to dataset d
pca_result <- pca(d)

# print(pca_result$eigenvectors)

pc_df <- as.data.frame(pca_result$transformed_data)
colnames(pc_df) <- paste0("PC", 1:ncol(pc_df))
# print(pca_result$eigenvalues)
variance_explained <- pca_result$eigenvalues / sum(pca_result$eigenvalues)
png("unit3/output/p2_manual_screeplot.png")
plot(variance_explained, type = "b", main = "Scree Plot", 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained")
dev.off()


png("unit3/output/p2_manual_pca.png")
plot(pc_df, main = "Projection onto PC1 and PC2")
dev.off()
# pairs(pc_df[, 1:2], main = "Pairwise Scatterplots of Principal Components")

png("unit3/output/p2_manual_pairwise.png")
pc_df$Species <- iris$Species
pairs(pc_df[, 1:4],col = as.numeric(pc_df$Species), main = "Pairwise Scatterplots of Principal Components")
dev.off()
# biplot(pca_result$transformed_data, pca_result$eigenvectors,
#     main = "PCA Biplot (New Coordinate System)"
# )



revert_data <- function(origin_data,pca_result) {
    # Step 1: Multiply PC scores by the transpose of eigenvectors
    X_scaled_reconstructed <- pca_result$transformed_data %*% t(pca_result$eigenvectors)

    # Step 2: Rescale back to original scale (undo standardization)
    X_reconstructed <- sweep(X_scaled_reconstructed, 2, attr(pca_result$data_scaled, "scaled:scale"), "*")
    X_reconstructed <- sweep(X_reconstructed, 2, attr(pca_result$data_scaled, "scaled:center"), "+")

    # Convert to dataframe
    X_reconstructed <- as.data.frame(X_reconstructed)
    colnames(X_reconstructed) <- colnames(data) # Restore original column names

    # Print reconstructed data (should be close to original data)
    # print(X_reconstructed)

    # Compare original and reconstructed data
    original_vs_reconstructed <- cbind(origin_data, X_reconstructed)
    colnames(original_vs_reconstructed) <- c("X_Original", "Y_Original",
                                            "X_Reconstructed", "y_Reconstructed")

    print(original_vs_reconstructed)


}
# revert_data(d,pca_result)
