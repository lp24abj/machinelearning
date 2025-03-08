set.seed(123)
library(class)
# Shuffle data
iris_shuffle <- iris[sample(1:nrow(iris)), ]

# Split into training and testing sets (80% training, 20% testing)
train_index <- 1:(0.8 * nrow(iris))
train_data <- iris_shuffle[train_index, 1:2]  # Selecting only feature columns
print(train_data)
test_data <- iris_shuffle[-train_index, 1:2]
train_labels <- iris_shuffle[train_index, 5]  # Target variable (Species)
test_labels <- iris_shuffle[-train_index, 5]

k <- 5  # Choose the number of neighbors
predicted_labels <- knn(train_data, test_data, train_labels, k = k)


# Generate a grid of points in the range of V1 and V2
x_range <- seq(min(train_data$Sepal.Length) - 0.1, max(train_data$Sepal.Length) + 0.1, by = 0.01)
y_range <- seq(min(train_data$Sepal.Width) - 0.1, max(train_data$Sepal.Width) + 0.1, by = 0.01)

grid_points <- expand.grid(V1 = x_range, V2 = y_range)
gpoint_predicted_labels <- knn(train_data, grid_points, train_labels, k = k)

#Advance to performance metrics
accuracy <- sum(predicted_labels == test_labels) / length(test_labels)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

png("unit4/output/p3_kmearestneighbour.png", width = 1000, height = 1000)
par(mfrow = c(2, 2))
plot(train_data, pch = 19, cex = 2, col = ifelse(train_labels == "setosa", "#01015c", ifelse(train_labels == "versicolor", "#8a0202", "#019201")), xlab = "Sepal Length", ylab = "Sepal Width", main = "Training data")

plot(train_data, pch = 19, cex = 2, col = ifelse(train_labels == "setosa", "#01015c", ifelse(train_labels == "versicolor", "#8a0202", "#019201")), xlab = "Sepal Length", ylab = "Sepal Width", main = "with Testing data")
points(test_data, pch = 19, cex = 2, col = ifelse(predicted_labels == "setosa", "#8181f5", ifelse(predicted_labels == "versicolor", "#f47474", "#75ec75")), add = TRUE)

plot(grid_points, pch = 19, cex = 0.5, col = ifelse(gpoint_predicted_labels == "setosa", "#babaf7", ifelse(gpoint_predicted_labels == "versicolor", "#f8b7b7", "#baf9ba")), xlab = "Sepal Length", ylab = "Sepal Width", main = "Grid points")
points(train_data, pch = 19, cex = 2, col = ifelse(train_labels == "setosa", "#01015c", ifelse(train_labels == "versicolor", "#8a0202", "#019201")), add = TRUE)
points(test_data, pch = 19, cex = 2, col = ifelse(predicted_labels == "setosa", "#5050f7", ifelse(predicted_labels == "versicolor", "#fb5959", "#4cf54c")), add = TRUE)

dev.off()

