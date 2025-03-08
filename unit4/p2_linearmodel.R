# data <- read.csv("unit4/data/spiral_labelled.csv", header = TRUE)
data <- matrix(c(1, 1, 2, 1, 3, 2, 2.1, 3, 3, 3), ncol = 2, byrow = TRUE)
df <- as.data.frame(data)
# print(df)
# Create the vector y
label <- c(0, 0, 0, 1, 1)

# cal the coefficients vector
# linearmodel <- lm(V2 ~ V1, data = df)
linearmodel <- glm(label ~ V1 + V2, data = df, family = binomial(link = "logit"))
summary(linearmodel)
coefficients <- linearmodel$coefficients
print(coefficients)

beta0 <- coefficients[1]
beta1 <- coefficients[2]
beta2 <- coefficients[3]


# Compute decision boundary parameters
slope <- -beta1 / beta2
intercept <- -beta0 / beta2

cat("beta0:", beta0, "\n")
cat("beta1:", beta1, "\n")
cat("beta2:", beta2, "\n")
cat("Equation: x2 =", intercept, "+", slope, "* x1", "\n")

x <- c(1, 2, 3)
y <- c(intercept + slope * x[1], intercept + slope * x[2], intercept + slope * x[3])
# print(y)
# point <- c(2.5, 1.3, 1.5, 2.3, 2.3, 2.7, 2.7, 1, 2.5, 2.5,1.7,1.3)
# point <- matrix(point, ncol = 2, byrow = TRUE)

new_data <- data.frame(
  V1 = c(2.5, 1.5, 2.3, 2.7, 2.5, 1.7),
  V2 = c(1.3, 2.3, 2.7, 1, 2.5, 1.3)
)
#
threshold <- 0.5
#manually calculate the predicted values
# predicted_y <- beta0 + beta1 * new_data$V1 + beta2 * new_data$V2
# predicted_value <- 1 / (1 + exp(-predicted_y))
# print(predicted_value)
# predicted_labels <- ifelse(predicted_value > threshold, 1, 0)

predicted_values <- predict(linearmodel, new_data, type = "response")
cat("Predicted value:", predicted_values, "\n")
predicted_labels <- ifelse(predicted_values > threshold, 1, 0)
cat("Predicted labels:", predicted_labels, "\n")

# Generate a grid of points in the range of V1 and V2
x_range <- seq(min(df$V1) - 0.1, max(df$V1) + 0.1, length.out = 100)
y_range <- seq(min(df$V2) - 0.1, max(df$V2) + 0.1, length.out = 100)

grid_points <- expand.grid(V1 = x_range, V2 = y_range)
gpoint_predicted_values <- predict(linearmodel, grid_points, type = "response")
gpoint_predicted_labels <- ifelse(gpoint_predicted_values > threshold, 1, 0)

png("unit4/output/p2_linearmodel.png", width = 1600, height = 800)
par(mfrow = c(1, 2))
plot(df$V1, df$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
# Add the grid points with color-coded predicted classes
points(grid_points$V1, grid_points$V2, col = ifelse(gpoint_predicted_labels == 0, "#a8a8f9", "#f5a9a9"), pch = 15, cex = 0.5)
points(new_data, col = ifelse(predicted_labels == 0, "#a8a8f9", "#f5a9a9"), pch = 19, cex = 2)
abline(a = intercept, b = slope, col = "darkgreen", lwd = 2)  # Line equation y = mx + c
points(x, y, col = "darkgreen", pch = 19, cex = 2)




plot(df$V1, df$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
# Add the grid points with color-coded predicted classes

image(x_range, y_range, 
  matrix(as.numeric(gpoint_predicted_labels), 
  nrow = length(x_range)), 
  col = c("#a8a8f9", "#f5a9a9"), 
  add = TRUE)

points(df$V1, df$V2, pch = 19, cex = 2, col = ifelse(label == 0, "blue", "red"))
points(new_data, col = ifelse(predicted_labels == 0, "#6a6af1", "#fe5d5d"), pch = 19, cex = 2)
abline(a = intercept, b = slope, col = "darkgreen", lwd = 2)  # Line equation y = mx + c
points(x, y, col = "darkgreen", pch = 19, cex = 2)
dev.off()
