if (!require("png")) install.packages("png")
library(png)

# Load the image
image_path <- "unit2/data/test_image_data1.png"  # Update this path
img <- readPNG(image_path)

sclFunc <- function(points,weights, iterations,learning_rate) {
# points(centers[, 1], centers[, 2], col = 'gray', pch = 16, cex = 2)
  # Simple Competitive Learning Algorithm
  for (iter in 1:iterations) {
    for (i in 1:nrow(points)) {
      distances <- sqrt((points$x[i] - weights[, 1])^2 + (points$y[i] - weights[, 2])^2)
      winner <- which.min(distances)
      # Update the winner center towards the input point
      weights[winner, 1] <- weights[winner, 1] + learning_rate * (points$x[i] - weights[winner, 1])
      weights[winner, 2] <- weights[winner, 2] + learning_rate * (points$y[i] - weights[winner, 2])
    }
   }
}

# Check if the image has alpha transparency (4th channel)
if (dim(img)[3] == 4) img <- img[, , 1:3]

# Extract dimensions
height <- dim(img)[1]
width <- dim(img)[2]

# Create a data frame with pixel RGB values
pixels <- data.frame(
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

# Run K-means clustering (set k for the number of clusters)
set.seed(123)  # For reproducibility
k <- 8  # Number of colors

kmeans_result <- kmeans(pixels, centers = k)
#Replace pixel colors with cluster centroids
quantized_colors <- kmeans_result$centers[kmeans_result$cluster, ]

# Reshape quantized colors back to an image format
quantized_image <- array(quantized_colors, dim = c(height, width, 3))

# epochs <- 1
# weights <- matrix(runif(k * 3), nrow = k, ncol = 3)
# sclFunc(pixels,weights, epochs, 0.1)
# # Assign each pixel to its closest cluster weight
# quantized_colors <- apply(pixels, 1, function(pixel) {
#   distances <- apply(weights, 1, function(w) sqrt(sum((pixel - w)^2)))
#   weights[which.min(distances), ]
# })

# # Reshape the quantized colors back to image format
# quantized_image <- array(quantized_colors, dim = c(height, width, 3))



# Write the quantized image back to a file
output_path <- "unit2/output/quantized_image.png"
writePNG(quantized_image, target = output_path)

cat("Quantized image saved to:", output_path)


