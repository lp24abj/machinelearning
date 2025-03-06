sclFunc <- function(data, codevectors, iterations, learning_rate) {
  # Plot the points
  plot(data,
    main = "Data Points",
    xlab = "X-axis",
    ylab = "Y-axis",
    pch = 16,
    cex = 2,
    col = "blue"
  )

  points(codevectors, col = "black", pch = 8, cex = 2)
  previous_centers <- codevectors
  # Simple Competitive Learning Algorithm
  for (iter in 1:iterations) {
    for (i in 1:nrow(data)) {
      # Calculate Euclidean distance from point to each center
      distances <- sqrt((data$x[i] - codevectors[, 1])^2 + (data$y[i] - codevectors[, 2])^2)
      # Find the index of the closest center
      winner <- which.min(distances)
      # Update the winner center towards the input point
      codevectors[winner, 1] <- codevectors[winner, 1] + learning_rate * (data$x[i] - codevectors[winner, 1])
      codevectors[winner, 2] <- codevectors[winner, 2] + learning_rate * (data$y[i] - codevectors[winner, 2])

      arrows(previous_centers[winner, 1], previous_centers[winner, 2],
        codevectors[winner, 1], codevectors[winner, 2],
        col = "lightcoral",
        length = 0.2,
        lwd = 2
      )
      # Update the previous centers
      previous_centers <- codevectors
    }
  }
  # # Visualising the results
  points(codevectors[, 1], codevectors[, 2],
    col = "red",
    pch = 19,
    cex = 3
  )
  grid()
}
# Create data

dataset <- read.table("unit2/data/kmeansdata01.csv", sep = ",", header = TRUE)
data <- dataset[, 1:2]
k <- 3
codevectors <- data[sample(1:nrow(data), k), ]

iterations <- 1 # epoch
learning_rate <- 1.2 # lamda
# Show Simple Competitive Learning
set.seed(123)
png("unit2/output/simplecl.png")
sclFunc(data, codevectors, iterations, learning_rate)
dev.off()
