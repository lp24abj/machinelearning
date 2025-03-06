draw_axes <- function(datax, datay) {
    max_x <- max(abs(datax))
    max_y <- max(abs(datay))
    print(length(datay))

    plot(NA, xlim = c(-max_x, max_x), ylim = c(-max_y, max_y), axes = F, ann = F)

    # Draw axes with arrows
    arrows(-max_x, 0, max_x, 0, col = "#000", length = 0.1, lwd = 2) # X-axis
    arrows(0, -max_y, 0, max_y, col = "#000", length = 0.1, lwd = 2) # Y-axis

    # Add points
    points(datax, datay, col = "blue", pch = 16, cex = 2)

    # Generate axis labels at even intervals
    x_ticks <- seq(-max_x, max_x, length.out = 5)
    y_ticks <- seq(-max_y, max_y, length.out = 5)

    # Remove 0 from labels to avoid overlap at origin
    x_ticks <- x_ticks[x_ticks != 0]
    y_ticks <- y_ticks[y_ticks != 0]

    # Add labels to X-axis
    text(x_ticks, rep(0, length(x_ticks)), labels = x_ticks, pos = 3, cex = 1.2) # pos = 3 means above

    # Add labels to Y-axis
    text(rep(0, length(y_ticks)), y_ticks, labels = y_ticks, pos = 4, cex = 1.2) # pos = 4 means right
}
pca <- function(data, n_components) {
    # Step 1: Standardize the Data
    data_scaled <- scale(d)
    meanx <- mean(d$x)
    meany <- mean(d$y)

    # plot(d$x, d$y, col = "red", pch = 16, cex = 2, )
    sdx <- sd(d$x)
    sdy <- sd(d$y)
    draw_axes(d$x, d$y)
    points(sdx, sdy, col = "yellow", pch = 16, cex = 2)
    points(meanx, meany, col = "black", pch = 16, cex = 2)
    # print(data_scaled)
    # Step 2: Compute the Covariance Matrix
    # plot(data_scaled, col = "red", pch = 16, cex = 2)
    # points(meanx, meany, col = "black", pch = 16, cex = 2)
    covariance_matrix <- cov(data_scaled)
    # print(covariance_matrix)
    # plot(covariance_matrix, col = "blue", pch = 16, cex = 2)





    # Step 3: Compute the Eigenvectors and Eigenvalues
    eig <- eigen(covariance_matrix)
    eigenvalues <- eig$values
    eigenvectors <- eig$vectors
    print(eigenvalues)
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
    # arrows(0, 0, eigenvectors[1, 1], eigenvectors[2, 1], col = "blue", length = 0.1)
    # arrows(0, 0, eigenvectors[1, 2], eigenvectors[2, 2], col = "red", length = 0.1)
    return(list(
        transformed_data = pca_transformed,
        eigenvalues = eigenvalues,
        eigenvectors = eigenvectors
    ))
}

# data <- iris[, 1:2]
# data_scaled <- scale(data)
# print(data_scaled)
# covariance_matrix <- cov(data_scaled)
# print(covariance_matrix)
# Step 1: Load the Data
d <- expand.grid(x = 1:10, y = 1:10)
# number of columns is 2 and number of rows is 100
# Scale the data
d$x <- 2 * d$x

png("unit3/output/pca.png")
# par(mfrow = c(2, 2))


# Apply PCA to dataset d
pca_result <- pca(d, 2)

# Print Results
print("Transformed Data (First 5 Rows):")
print(head(pca_result$transformed_data, 5))

print("Eigenvalues:")
print(pca_result$eigenvalues)

print("Eigenvectors:")
print(pca_result$eigenvectors)

# plot(pca_result$eigenvectors, col = "blue", pch = 16, cex = 2)
dev.off()
