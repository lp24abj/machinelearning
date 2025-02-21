#Create data

#Example 1
#Load data from csv file
data <- read.table("unit2/data/kmeansdata01.csv", sep = ",", header = TRUE)
# #Select columns 2 and 3
points <- data[, 1:2]

# #Show data in chart
# plot(example1_data,
#     main = "Scatter Plot of 10 Random Points", 
#     xlab = "X-axis", 
#     ylab = "Y-axis", 
#     pch = 16, 
#     cex = 2,
#     col = "blue")

#Example 2
# Assigning data
set.seed(123)  # Set seed for reproducibility
# x <- runif(10, min = 0, max = 10)  # Random x coordinates between 0 and 10
# y <- runif(10, min = 0, max = 10)  # Random y coordinates between 0 and 10

# Assigning data statically
# x <- c(5, 3.5, 3.7, 4, 5, 3, 3,4.7)
# y <- c(2, 1, 4, 1, 1.5, 1.7, 4,2.8)

# Add to data frame
# points <- data.frame(x, y)
png("unit2/output/k_mean.png")
# Plot the points
plot(points, main = "List of Points", 
    xlab = "X-axis", 
    ylab = "Y-axis", 
    pch = 16, 
    cex = 2,
    col = "blue")

# Add grid lines for better visualization
grid()

# Calculate k-means
# set k-means k
k <- 2
# Applying k-means with k=2
kmeans_result <- kmeans(points, centers = k)

# Make line to cluster centers
for (i in 1:nrow(points)) {
  center <- kmeans_result$centers[kmeans_result$cluster[i], ]
  lines(c(points$x[i], center[1]), c(points$y[i], center[2]), col = "gray")
}

# Visualising the results
points(kmeans_result$centers, col = 'red', pch = 16, cex = 2)
dev.off()