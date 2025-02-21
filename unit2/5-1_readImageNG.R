# Load required libraries
if (!require("png")) install.packages("png")
library(png)

# Load the image
image_path <- "unit2/data/test_image_data1.png"  # Update with your image path
img <- readPNG(image_path)

# Remove alpha channel if present
if (dim(img)[3] == 4) img <- img[, , 1:3]

# Extract dimensions
height <- dim(img)[1]
width <- dim(img)[2]

# Create data frame of pixel RGB values
pixels <- as.matrix(data.frame(
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
))

# Normalize pixel values (range 0-1)
pixels <- pixels / max(pixels)

# Initialize Neural Gas parameters
set.seed(42)
k <- 64  # Number of clusters
iterations <- 1000
learning_rate_initial <- 0.5
neighborhood_initial <- k / 2

# Initialize neuron weights randomly
weights <- matrix(runif(k * 3), nrow = k, ncol = 3)

# Neural Gas Algorithm
for (iteration in 1:iterations) {
  # Select a random pixel
  pixel <- pixels[sample(1:nrow(pixels), 1), ]

  # Compute distances between pixel and all neurons
  distances <- apply(weights, 1, function(w) sqrt(sum((pixel - w)^2)))

  # Rank neurons based on distances
  rank_indices <- order(distances)

  # Learning rate and neighborhood decay
  eta <- learning_rate_initial * exp(-iteration / iterations)
  lambda <- neighborhood_initial * exp(-iteration / iterations)

  # Update weights
  for (j in 1:k) {
    rank <- which(rank_indices == j) - 1  # Rank index starts from 0
    h_j <- exp(-rank / lambda)  # Neighborhood function
    weights[j, ] <- weights[j, ] + eta * h_j * (pixel - weights[j, ])
  }
}

# Assign each pixel to its closest cluster weight
quantized_colors <- apply(pixels, 1, function(pixel) {
  distances <- apply(weights, 1, function(w) sqrt(sum((pixel - w)^2)))
  weights[which.min(distances), ]
})

# Reshape the quantized colors back to image format
quantized_image <- array(quantized_colors, dim = c(height, width, 3))

# Write the quantized image back to a file
output_path <- "output/unit2/neural_gas_quantized_image.png"
writePNG(quantized_image, target = output_path)

cat("Quantized image saved to:", output_path)
