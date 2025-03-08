data <- read.csv("unit4/data/spiral_labelled.csv", header = TRUE)
df <- as.data.frame(data)

# cal the coefficients vector
# linearmodel <- lm(V2 ~ V1, data = df)
linearmodel <- glm(cls ~ y + x, data = df, family = binomial(link = "logit"))
summary(linearmodel)

coefficients <- linearmodel$coefficients
print(coefficients)

beta0 <- coefficients[1]
beta1 <- coefficients[2]
beta2 <- coefficients[3]


# Compute decision boundary parameters
slope <- -beta2 / beta1
intercept <- -beta0 / beta1

cat("beta0:", beta0, "\n")
cat("beta1:", beta1, "\n")
cat("beta2:", beta2, "\n")
cat("Equation: x2 =", intercept, "+", slope, "* x1", "\n")

threshold <- 0.5
# Generate a grid of points in the range of V1 and V2
x_range <- seq(min(df$x) - 0.1, max(df$x) + 0.1, length.out = 100)
y_range <- seq(min(df$y) - 0.1, max(df$y) + 0.1, length.out = 100)

grid_points <- expand.grid(x = x_range, y = y_range)
gpoint_predicted_values <- predict(linearmodel, grid_points, type = "response")
gpoint_predicted_labels <- ifelse(gpoint_predicted_values > threshold, 1, 0)

png("unit4/output/p2_linearmodel_spiraldata.png", width = 1600, height = 800)
par(mfrow = c(1, 2))
plot(df$x, df$y, pch = 19, cex = 2, col = ifelse(df$cls == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
# Add the grid points with color-coded predicted classes
points(grid_points$x, grid_points$y, col = ifelse(gpoint_predicted_labels == 0, "#a8a8f9", "#f5a9a9"), pch = 15, cex = 0.5)
# points(new_data, col = ifelse(predicted_labels == 0, "#a8a8f9", "#f5a9a9"), pch = 19, cex = 2)
abline(a = intercept, b = slope, col = "darkgreen", lwd = 2)  # Line equation y = mx + c
# points(x, y, col = "darkgreen", pch = 19, cex = 2)




plot(df$x, df$y, pch = 19, cex = 2, col = ifelse(df$cls == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
# Add the grid points with color-coded predicted classes

image(x_range, y_range, 
  matrix(as.numeric(gpoint_predicted_labels), 
  nrow = length(x_range)), 
  col = c("#a8a8f9", "#f5a9a9"), 
  add = TRUE)

points(df$x, df$y, pch = 19, cex = 2, col = ifelse(df$cls == 0, "blue", "red"))
# points(new_data, col = ifelse(predicted_labels == 0, "#6a6af1", "#fe5d5d"), pch = 19, cex = 2)
abline(a = intercept, b = slope, col = "darkgreen", lwd = 2)  # Line equation y = mx + c
dev.off()
