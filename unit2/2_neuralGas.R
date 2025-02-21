neural_gas <- function(data, num_nodes, max_epochs, learning_rate_initial, learning_rate_final, lambda_initial, lambda_final) {
  # Number of data points
  num_data_points <- nrow(data)

  # Initialize weight vectors randomly within the data range
  # weights <- matrix(runif(num_nodes * ncol(data), min = min(data), max = max(data)),
  #                    nrow = num_nodes, ncol = ncol(data))

  weights <- matrix(c(3, 4.7, 3.6, 3), ncol = 2)
  points(weights[, 1], weights[, 2], col = "gray", pch = 16, cex = 2)


  # Learning rate and lambda decay functions
  decay <- function(initial_value, final_value, epoch, max_epochs) {
    initial_value * ((final_value / initial_value)^(epoch / max_epochs))
    # (3 * (0.1 / 3) ^ (1 / 100)) = 0.1
    # (0,5 * (0,01 / 0,5) ^ (2 / 100)) = 0.01
  }

  # Main training loop
  for (epoch in 1:max_epochs) {
    # Shuffle the data
    data <- data[sample(1:num_data_points), ]

    # Compute current learning rate and lambda
    learning_rate <- decay(learning_rate_initial, learning_rate_final, epoch, max_epochs)
    lambda <- decay(lambda_initial, lambda_final, epoch, max_epochs)

    # Loop through each data point
    for (i in 1:num_data_points) {
      input <- data[i, ]
      text(data[i, 1], data[i, 2] - 0.1, labels = i, cex = 1.5)

      # Compute distances between input and all weight vectors
      distances <- apply(weights, 1, function(w) sum((w - input)^2))

      # Sort nodes by their distance to the input
      sorted_indices <- order(distances)

      # Update weight vectors
      for (j in 1:num_nodes) {
        rank <- which(sorted_indices == j) - 1
        influence <- exp(-rank / lambda)
        old_weights <- weights
        weights[j, ] <- old_weights[j, ] + learning_rate * influence * (input - old_weights[j, ])
        arrows(old_weights[j, 1], old_weights[j, 2],
          weights[j, 1], weights[j, 2],
          col = "lightcoral",
          length = 0.2,
          lwd = 2
        )
        points(weights[j, 1], weights[j, 2],
          col = "orange",
          pch = 16,
          cex = 3
        )
      }
    }

    # Progress report
    if (epoch %% 10 == 0) {
      cat("Epoch:", epoch, "\n")
    }
  }

  # Return the final weight vectors
  return(weights)
}

# Example Usage
set.seed(42) # For reproducibility

# Generate synthetic 2D data
# num_points <- 300
# data <- cbind(runif(num_points, -1, 1), runif(num_points, -1, 1))

# Assigning data statically
x <- c(5, 3.5, 3.7, 4, 5, 3, 3, 4.7)
y <- c(2, 1, 4, 1, 1.5, 1.7, 4, 2.8)

# Add to data frame
data <- data.matrix(data.frame(x, y), rownames.force = NA)
# print(data)
# Neural Gas parameters
num_nodes <- 2
max_epochs <- 1
learning_rate_initial <- 1
learning_rate_final <- 0.3
lambda_initial <- 3
lambda_final <- 0.5
png("unit2/output/neuralGas.png")
# Plot the result
plot(data, col = "blue", pch = 19, xlab = "X1", ylab = "X2", main = "Neural Gas Clustering", cex = 3)

# Run the algorithm
final_weights <- neural_gas(data, num_nodes, max_epochs, learning_rate_initial, learning_rate_final, lambda_initial, lambda_final)

points(final_weights, col = "red", pch = 19, cex = 3)
grid()
dev.off()
