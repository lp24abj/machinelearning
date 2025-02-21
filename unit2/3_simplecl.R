sclFunc <- function(points, iterations,learning_rate) {
  # random_centers <- round(runif(4, min = 1, max = 5),0)
  # Plot the points
  plot(points, main = "List of Points", 
      xlab = "X-axis", 
      ylab = "Y-axis", 
      pch = 16, 
      cex = 2,
      col = "blue")

  # print(random_centers)
  centers <- matrix(c(0,3,3,3), ncol = 2)
  # print(centers)
  points(centers[, 1], centers[, 2], col = 'gray', pch = 16, cex = 2)

  # Define the number of iterations and learning rate


  # Store the previous positions of centers to visualize the movement
  previous_centers <- centers


  # Simple Competitive Learning Algorithm
  for (iter in 1:iterations) {
    for (i in 1:nrow(points)) {
      text(points$x[i], points$y[i] - 0.1, labels = i, cex = 1.5)
      # Calculate Euclidean distance from point to each center
      distances <- sqrt((points$x[i] - centers[, 1])^2 + (points$y[i] - centers[, 2])^2)
      # print(distances)
      # Find the index of the closest center
      winner <- which.min(distances)
      # print(winner)
      #debug winner point
      # print(distances)
      # points(points$x[i], points$y[i],
      #       col = "orange", 
      #       pch = 16, 
      #       cex = 3)

      # Update the winner center towards the input point
      centers[winner, 1] <- centers[winner, 1] + learning_rate * (points$x[i] - centers[winner, 1])
      centers[winner, 2] <- centers[winner, 2] + learning_rate * (points$y[i] - centers[winner, 2])
      # print('----new center')
      # print(centers[winner, 1])
      # print(centers[winner, 2])
      # print('-end---new center')
      # points(centers[winner, 1], centers[winner, 2],
      #       col = "orange", 
      #       pch = 16, 
      #       cex = 3)
      # arrows(previous_centers[winner, 1],previous_centers[winner, 2]
      #       ,centers[winner,1], centers[winner,2],
      #       col = "lightcoral",
      #       length = 0.2,
      #       lwd = 2)
      # Update the previous centers
      previous_centers <- centers
    }
    # print (centers)
    # Show the movement of centers
    # for (j in 1:nrow(centers)) {
    #   arrows(previous_centers[j, 1],previous_centers[j, 2]
    #         ,centers[j,1], centers[j,2],
    #         col = "lightcoral",
    #         length = 0.2,
    #         lwd = 2)
    #   # lines(c(previous_centers[j, 1], centers[j, 1]),
    #   #  c(previous_centers[j, 2],centers[j, 2]),
    #   #       col = "lightcoral")  # Light red color for the movement lines
    # }

    # Update the previous centers
    # previous_centers <- centers
  }


  # Make line to cluster centers
        # for (i in 1:nrow(points)) {
        #   center <- kmeans_result$centers[kmeans_result$cluster[i], ]
        #   lines(c(points$x[i], center[1]), c(points$y[i], center[2]), col = "gray")
        # }

        # # Visualising the results
        points(centers[, 1], centers[, 2], 
              col = "red", 
              pch = 19, 
              cex = 3)
        grid()
}
#Create data
# Assigning data statically
# x <- c(5, 3.5, 3.7, 4, 5, 3, 3,4.7)
# y <- c(2, 1, 4, 1, 1.5, 1.7, 4,2.8)
# # Add to data frame
# points <- data.frame(x, y)

data <- read.table("unit2/data/kmeansdata01.csv", sep = ",", header = TRUE)
# #Select columns 2 and 3
points <- data[, 1:2]

iterations <- 50 #epoch
learning_rate <- 0.4 #lamda
# Show Simple Competitive Learning
# Initialize 2 random cluster centers (for simplicity)
set.seed(123)
png("unit2/output/simplecl.png")
sclFunc(points, iterations, learning_rate)
dev.off()