cal_ols_multiple <- function(data, label,threshold) {
  # Add a column of ones for the intercept term
  data <- cbind(1, data)
  # print(data)
  # print(t(data))
  # print(t(data)%*% data)
  # print(solve(t(data)%*% data))

  coefficient <- solve(t(data) %*% data) %*% t(data) %*% label

  # print(coefficient)

  beta0 <- coefficient[1]
  beta1 <- coefficient[2]
  beta2 <- coefficient[3]

  slope <- -beta1 / beta2
  intercept <- -(beta0-threshold) / beta2

  return(list(coefficient = coefficient, slope = slope, intercept = intercept))
}
cal_ols_single <- function(data, label) {
  # Step 1: Compute means
  x_mean <- mean(x)
  y_mean <- mean(y)

  # Step 2: Compute beta1 (slope)
  beta1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)

  # Step 3: Compute beta0 (intercept)
  beta0 <- y_mean - beta1 * x_mean
  coefficient[1] <- beta0
  coefficient[2] <- beta1

  slope <- beta1
  intercept <- beta0
  return(list(coefficient = coefficient, slope = slope, intercept = intercept))
}


predict_manual <- function(new_X, coefficient) {
  new_X <- cbind(1, new_X) # Add intercept column
  return(as.vector(new_X %*% coefficient))
}
predict_single <- function(new_X, coefficient) {
  return(coefficient[1] + coefficient[2] * new_X)
}

# visualize the data
print_data <- function(data, predict_data, intercept, slope, label, x, y) {
  png("unit4/output/p2_linearmodel_manual.png", width = 1600, height = 800)
  par(mfrow = c(1, 2))
  plot(data$V1, data$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
  points(predict_data, col = ifelse(predicted_labels == 0, "#a8a8f9", "#f5a9a9"), pch = 19, cex = 2)
  abline(a = intercept, b = slope, col = "darkgreen", lwd = 2) # Line equation y = mx + c
  points(x, y, col = "darkgreen", pch = 19, cex = 2)


  plot(df$V1, df$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
  # Add the grid points with color-coded predicted classes
  image(x_range, y_range,
    matrix(as.numeric(gpoint_predicted_labels),
      nrow = length(x_range)
    ),
    col = c("#a8a8f9", "#f5a9a9"),
    add = TRUE
  )
  points(df$V1, df$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"))
  points(predict_data, col = ifelse(predicted_labels == 0, "#6a6af1", "#fe5d5d"), pch = 19, cex = 2)
  abline(a = intercept, b = slope, col = "#050505", lwd = 2)
  dev.off()
}






# data <- read.csv("unit4/data/spiral_labelled.csv", header = TRUE)
data <- matrix(c(1, 1, 2, 1, 3, 2, 2.1, 3, 3, 3), ncol = 2, byrow = TRUE)
df <- as.data.frame(data)
label <- c(0, 0, 0, 1, 1)
print(df)
threshold <- 0.3
linearmodel <- cal_ols_multiple(data, label,threshold)
# linearmodel_single <- cal_ols_single(data[, 1], data[, 2])
coefficient <- linearmodel$coefficient
intercept <- linearmodel$intercept
slope <- linearmodel$slope

print(intercept)
print(slope)




# Define coefficients (β values) - adjust these based on expectations
# beta <- c(0.5, -0.3)  # Example values for weights

# Compute classification scores
df$score <- coefficient[1] + df$V1 * coefficient[2] + df$V2 * coefficient[3]

# Print the dataframe with computed scores
print(df)

# Analyze the scores
df$predicted_label <- ifelse(df$score > threshold, 1, 0) # Binary classification threshold at 0
print(df)

# Check consistency with actual labels
consistency <- table(Predicted = df$predicted_label, Actual = label)
print(consistency)

# result <- sum(data %*% coefficient)
# cat("Result:", result, "\n")

predict_data <- matrix(c(2.5, 1.3, 1.5, 2.3, 2.3, 2.7, 2.7, 1, 2.5, 2.5, 1.7, 1.3), ncol = 2, byrow = TRUE)

predicted_values <- predict_manual(predict_data, coefficient)
cat("Predicted value:", predicted_values, "\n")
predicted_labels <- ifelse(predicted_values > threshold, 1, 0)
cat("Predicted labels:", predicted_labels, "\n")



x <- c(1, 2, 3)
# y <- c(intercept + coefficient[2] * x[1], intercept + coefficient[2] * x[2], intercept + coefficient[2] * x[3])

decision_boundary <- function(x1, coefficient) {
  (slope * x1 + intercept)# Solving for x2
}

y <- decision_boundary(x, coefficient)
print(y)

# manually calculate the predicted values
# predicted_y <- beta0 + beta1 * new_data$V1 + beta2 * new_data$V2
# predicted_value <- 1 / (1 + exp(-predicted_y))
# print(predicted_value)
# predicted_labels <- ifelse(predicted_value > threshold, 1, 0)



# Generate a grid of points in the range of V1 and V2
x_range <- seq(min(df$V1) - 0.1, max(df$V1) + 0.1, length.out = 100)
y_range <- seq(min(df$V2) - 0.1, max(df$V2) + 0.1, length.out = 100)

grid_points <- expand.grid(V1 = x_range, V2 = y_range)
gpoint_predicted_values <- predict_manual(as.matrix(grid_points), coefficient)
gpoint_predicted_labels <- ifelse(gpoint_predicted_values > threshold, 1, 0)

print_data(df, predict_data, intercept, slope, label, x, y)
