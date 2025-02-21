# result <- 21*37+3
# print("21*37+3")
# print(result)

# result2 <- sqrt(2)
# print("sqrt(2)")
# print(result2)

# formula <- 4-9/2
# result3 <- sqrt(formula)
# print("sqrt(4-9/2)")
# print(result3)


# formula2 <- 1-1/3-1/3-1/3
# print("1-1/3-1/3-1/3")
# print(formula2)


# formula3 <- 10^200 * 10^150
# print("10^200 * 10^150")
# print(formula3)

# x <- matrix(c(1,2,3), nrow = 3, ncol = 1)
# y <- matrix(c(2,1,-3), nrow = 3, ncol = 1)
# print(x)
# print(y)
# # x_transposed <- t(x)
# # print(x_transposed)
# # y_transposed <- t(y)
# # print(y_transposed)
# dot_product <- sum(x * y)
# print(dot_product)

m1 <- matrix(c(1,2,4,5), nrow = 2, ncol = 2 , byrow = TRUE)
m2 <- matrix(c(1,0,0,2), nrow = 2, ncol = 2, byrow = TRUE)

s <- m1 + m2
m <- m1 * m2
m2 <- m2 * m1


v <- m1 %*% m2
v2 <- m2 %*% m1
v3 <- t(m1) %*% m2
v4 <- m2 %*% t(m1)

# print(m)
# print(m2)
# print(t(m1))
# print(t(m2))
print('Result:')
print(s)
print('Result <Multi:')
# print(v)
# print(v2)
print('Result2:')
# print(t(v3))
# print(v4)
# A <- matrix(1:6, nrow = 2, ncol = 3)
# B <- matrix(6:1, nrow = 3, ncol = 2)

# result <- A %*% B
# print(A)
# print(B)
# # Print the result
# print(result)


# f <- function(x) {
#   return(x^2 -2*x)
# }

# x <- seq(-10, 10, by = 0.1)
# y <- f(x)

# plot(x, y, type = "l", col = "blue", lwd = 2, xlab = "x", ylab = "f(x)", main = "Plot of f(x) = x^2 - 2x")
# points(1, f(1), col = "red",pch=2)


# data <- read.table("disastersim01.csv", sep = ",", header = TRUE)
# red_points <- data[, 5] == 1
# plot(data[,1],data[,2],pch=19, xlab = "land x", ylab = "land y",col= "blue", main = "position of land")
# points(data[red_points, 1], data[red_points, 2], col = "red", pch = 19)

# data <- read.table("disastersim01.csv", sep = ",", header = TRUE)
# red_points <- data[, 5] == 1
# plot(data[,3],data[,4],pch=19, xlab = "parachuteSize", ylab = "Weight",col= "blue", main = "Parachute Size vs Weight")
# points(data[red_points, 3], data[red_points, 4], col = "red", pch = 19)