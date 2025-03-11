# Load necessary library
library(e1071)

# Generate example data
set.seed(123)
x1 <- rnorm(20, mean = 2, sd = 1)
x2 <- rnorm(20, mean = -2, sd = 1)
y <- factor(c(rep(1, 10), rep(-1, 10)))  # Class labels

# Create a 2D dataset (adding another feature)
x3 <- rnorm(20, mean = 2, sd = 1)  # Another random variable
x4 <- rnorm(20, mean = -2, sd = 1)

# Combine into a data frame
data <- data.frame(x1 = c(x1, x2), x2 = c(x3, x4), y = y)

# Train an SVM model using a linear kernel
svm_model <- svm(y ~ ., data = data, kernel = "linear", scale = FALSE)
png("test_svm.png", width = 1600, height = 800)
# Plot the decision boundary
plot(svm_model, data)

# Print the support vectors
print(svm_model$index)  # Indices of support vectors
dev.off()
