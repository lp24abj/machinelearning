# Load necessary library
library(ggplot2)
library(cluster)

plot_origin_datapoint <- function(data, codevector) {
  # Plot the data points
  plot(
    data$x,
    data$y,
    col = "blue",
    xlab = "X-axis",
    ylab = "Y-axis",
    main = "Data points",
    pch = 16,
    cex = 2,
  )
  # Plot the codevectors
  for (i in 1:nrow(codevector)) {
    points(codevector[i, 1], codevector[i, 2], col = "black", pch = 8, cex = 3)
  }
}


display_kmeans_plot <- function(data, codevector, iter, chart_title) {
  # calculate the kmeans
  kmeans_result <- kmeans(data,
    centers = codevector,
    iter.max = iter,
    trace = TRUE
  )
  colors_list <- c(
    "#E69F00", # Orange
    "#56B4E9", # Sky Blue
    "#009E73", # Green
    "#F0E442", # Yellow
    "#0072B2", # Blue
    "#D55E00", # Red
    "#CC79A7", # Pink
    "#999999", # Gray
    "#6600CC", # Purple
    "#00CCCC" # Cyan
  )
  # Plot the data points
  plot(
    data$x,
    data$y,
    col = colors_list[kmeans_result$cluster],
    xlab = "X-axis",
    ylab = "Y-axis",
    main = chart_title,
    pch = 16,
    cex = 2,
  )
  # Plot the centroids
  points(kmeans_result$centers,
    col = "black",
    pch = 8,
    cex = 3
  ) # Centroids as black stars
}

# Function to compute WSS for different values of k
compute_wss <- function(data, max_k = 10) {
  wss_values <- numeric(max_k)

  for (k in 1:max_k) {
    km <- kmeans(data, centers = k, nstart = 10) # Run K-means
    wss_values[k] <- km$tot.withinss # Store total WSS
  }

  return(wss_values)
}
elbow_finder <- function(x, y) {

  # Find the elbow point in the curve with method "maximum curvature"
  n <- length(y)
  curvature <- numeric(n - 2)

  for (i in 2:(n - 1)) {
    a <- c(x[i - 1], y[i - 1])
    b <- c(x[i], y[i])
    c <- c(x[i + 1], y[i + 1])

    ab <- sqrt(sum((b - a)^2))
    bc <- sqrt(sum((c - b)^2))
    ac <- sqrt(sum((c - a)^2))

    s <- (ab + bc + ac) / 2
    area <- sqrt(s * (s - ab) * (s - bc) * (s - ac))

    if (ab * bc * ac != 0) {
      curvature[i - 1] <- (4 * area) / (ab * bc * ac)
    } else {
      curvature[i - 1] <- 0
    }
  }

  elbow_index <- which.max(curvature) + 1
  return(elbow_index)
}





set.seed(123)
png("unit2/output/p2_k_mean.png")
# Plot the SOM results
par(mfrow = c(2, 2))

# Simple test
# x <- c(1, 2, 3, 5, 5, 6, 6)
# y <- c(1, 1, 1, 5, 6, 5, 6)
# data <- data.frame(x, y)
# codevector <- matrix(c(2, 2, 2, 3), nrow = 2, byrow = TRUE)

# dataset test
dataset <- read.table("unit2/data/kmeansdata04.csv", sep = ",", header = TRUE)
# #Select columns 2 and 3
data <- dataset[, 1:2]
k <- 9

# # Randomly select k data points as initial codevectors
codevector <- data[sample(1:nrow(data), k), ]
plot_origin_datapoint(data, codevector)
# display_kmeans_plot(data, codevector, 1)
display_kmeans_plot(data, codevector, 2, sprintf("K-means clustering, iter = %d", 2))
display_kmeans_plot(data, codevector, 10, sprintf("K-means clustering, iter = %d", 10))

codevector2 <- data[sample(1:nrow(data), k), ]
display_kmeans_plot(data, codevector2, 10, sprintf("Init new centroids, iter = %d", 10))



max_k <- 10
wss_values <- compute_wss(data, max_k)

# Create a dataframe for visualization
wss_df <- data.frame(k = 1:max_k, WSS = wss_values)
# Find the optimal K
optimal_k <- elbow_finder(1:max_k, wss_values)
print(paste("Optimal K:", optimal_k))
# Plot the Elbow Curve
ggplot(wss_df, aes(x = k, y = WSS)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Elbow Method for Optimal k",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares (WSS)"
  ) +
  theme_bw()
ggsave("unit2/output/p2_elbow_method.png", width = 6, height = 4, dpi = 300)

dev.off()
