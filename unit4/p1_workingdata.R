data <- matrix(c(1, 1, 2, 1, 3, 2, 2.1, 3, 3, 3), ncol = 2, byrow = TRUE)
df <- as.data.frame(data)

# Create the vector y
y <- c(0, 0, 0, 1, 1)

png("unit4/output/p1_workingdata.png", width = 800, height = 800)
plot(df$V1, df$V2, pch = 19,cex=2, col = ifelse(y == 0, "blue", "red"), xlab = "X", ylab = "Y", main = "Scatterplot of X and Y")
dev.off()