set.seed(123)
library(class)
data <- read.csv("unit4/data/spiral_labelled.csv")
train_data <- data[, c("x", "y")]
train_labels <- data$cls

# Generate a grid of points in the range of V1 and V2
x_range <- seq(min(train_data$x) - 0.1, max(train_data$x) + 0.1, by = 0.01)
y_range <- seq(min(train_data$y) - 0.1, max(train_data$y) + 0.1, by = 0.01)

k=5

grid_points <- expand.grid(V1 = x_range, V2 = y_range)
gpoint_predicted_labels <- knn(train_data, grid_points, train_labels, k = k)

# Create a matrix
# Convert predictions to numeric values (1 for Red, 2 for Blue)
z_matrix <- matrix(as.numeric(gpoint_predicted_labels), nrow = length(x_range))
# Define colors
colors <- c("#8282f9", "#fa7373")
png("unit4/output/p4_contour_plot.png", width = 800, height = 800)
# Plot decision boundary
image(x_range, y_range, z_matrix, col = colors, add= FALSE, xlab = "x", ylab = "y", main = "Spinral data with decision boundary")
points(train_data, pch = 19, cex = 2, col = ifelse(train_labels == 0, "blue", "red"))
dev.off()