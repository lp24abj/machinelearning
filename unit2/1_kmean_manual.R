#Create data

#Example 1
#Load data from csv file
data <- read.table("unit2/data/kmeansdata01.csv", sep = ",", header = TRUE)
# #Select columns 2 and 3
points <- data[, 1:2]
# Assigning data
set.seed(123)  # Set seed for reproducibility

png("unit2/output/k_mean_manually.png")
# Plot the SOM results
par(mfrow = c(1, 2))
# Plot the points
plot(points, main = "List of Points", 
    xlab = "X-axis", 
    ylab = "Y-axis", 
    pch = 16,
    cex = 2,
    col = "blue")

# Add grid lines for better visualization
grid()






kmeans_custom <- function(data, k, max_iter = 100, tol = 1e-4) {
  # Randomly select k initial centroids
  set.seed(42)  # Ensure reproducibility

  # Sample data
x <- c()
y <- c()
  centroids <- data[sample(1:nrow(data), k), ]


  
  # Initialize variables
  clusters <- rep(0, nrow(data))
  iter <- 0

  repeat {
    x <- c(x, iter)
    iter <- iter + 1
    old_centroids <- centroids
    
    # Assign each point to the nearest centroid
    for (i in 1:nrow(data)) {
      distances <- apply(centroids, 1, function(centroid) sum((data[i, ] - centroid)^2))
      clusters[i] <- which.min(distances)
      
    }
    
    # Compute new centroids
    for (j in 1:k) {
      if (sum(clusters == j) > 0) {  # Avoid empty clusters
        new_centre <- colMeans(data[clusters == j, ])
        distance_from <- sum(rowSums((new_centre - centroids[j, ])^2))
        centroids[j, ] <- new_centre
        y <- c(y, distance_from)
      }
    }
    
    # Check for convergence (if centroids do not change significantly)
    # max_iter is the maximum number of iterations
    # tol is the tolerance level
    if (max(abs(old_centroids - centroids)) < tol || iter >= max_iter) {
      break
    }
  }
  
  return(list(centers = centroids, cluster = clusters, iterations = iter,x=x,y=y))
}






# Calculate k-means
# set k-means k
k <- 1
# Applying k-means with k=2
# kmeans_result <- kmeans(points, centers = k)
kmeans_result <- kmeans_custom(points, k)
# Make line to cluster centers
for (i in 1:nrow(points)) {
  center <- kmeans_result$centers[kmeans_result$cluster[i], ]
  lines(c(points$x[i], center[1]), c(points$y[i], center[2]), col = "gray")
}

# Visualising the results
points(kmeans_result$centers, col = 'red', pch = 16, cex = 2)
print(kmeans_result$y)
# Create a simple line chart
plot(kmeans_result$x, kmeans_result$y, type = "o", col = "blue", lwd = 2, xlab = "Interaction ", ylab = "Y-axis", main = "K-means Clustering")

dev.off()