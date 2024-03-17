library(KernSmooth)

# sorting data frame according x_values means eruptions
library(MASS)

data <- Boston[order(Boston$lstat),]
x <- data$lstat
y <- data$medv

K <- function(u)
{
  return(dnorm(u))
}

#mean approach

mean_approach_est <- function(theta, x, y, h, x_0) {
  theta_0 <- theta[1]
  theta_1 <- theta[2]
  theta_2 <- theta[3]
  n <- length(x)
  result <- 1/n * sum((y - theta_0 - theta_1 * (x - x_0) - theta_2 * (x - x_0)^2/2)^2 * K((x - x_0)/h))
  return(result)
}


initial_guess <- c(0, 0, 0)  # Initial guess for theta_0, theta_1, and theta_2
h <- 0.85  # Bandwidth parameter for the kernel
x_values <- x
m_hat_1 <- matrix(0,nrow = length(x_values), ncol = 3)
for (i in 1:length(x_values)) {
  x_0 <- x_values[i]
  opt_result <- optim(initial_guess, mean_approach_est, x = x, y = y, h = h, x_0 = x_0)
  m_hat_1[i,] <- c(opt_result$par[1],opt_result$par[2],opt_result$par[3])
}

colnames(m_hat_1) <- c("theta_0","theta_1","theta_2" )

#median approach
median_approach_est <- function(theta, x, y, h, x_0) {
  theta_0 <- theta[1]
  theta_1 <- theta[2]
  theta_2 <- theta[3]
  
  n <- length(x)
  result <- 1/n * sum(abs((y - theta_0 - theta_1 * (x - x_0) - theta_2 * ((x - x_0)^2)/2)) * K((x - x_0)/h))
  return(result)
}
m_hat_2 <- matrix(0,nrow = length(x_values), ncol = 3)
for (i in 1:length(x_values)) {
  x_0 <- x_values[i]
  opt_result <- optim(initial_guess, median_approach_est, x = x, y = y, h = h, x_0 = x_0)
  m_hat_2[i,] <- c(opt_result$par[1],opt_result$par[2],opt_result$par[3])
}

colnames(m_hat_2) <- c("theta_0","theta_1","theta_2" )

#plot
plot(x,y, xlab = "lstat", ylab = "medv", main = "Regression Plot")
model <- locpoly(x,y,bandwidth = 0.85)
lines(model, col = "red", lwd = 2)
lines(x_values, m_hat_1[,1], lwd = 2)
lines(x_values,m_hat_2[,1], type = "l", col = "blue", lwd = 2)
legend("topright",c("locpoly-inbuilt-est","mean_approach_est","median_approach_est"), fill = c("black","red","blue"))



#first_derivative
model2 <- locpoly(x,y, bandwidth = 0.85,drv = 1)
plot(model2, type = "l",main = "first derivative", lwd = 2, ylim = c(-7,18))
lines(x_values,m_hat_1[,2], col = "red", type = "l", lwd = 2)
lines(x_values,m_hat_2[,2], col = "blue", lwd =2)
legend("topright",c("locpoly-inbuilt-est","mean_approach_est","median_approach_est"), fill = c("black","red","blue"))


#Second_derivative
model3 <- locpoly(x,y,bandwidth = 0.85,drv = 2)
plot(model3, type = "l",main = "Second_derivative", lwd = 2, ylim = c(-5,18))
lines(x_values,m_hat_1[,3], col = "red", type = "l", lwd = 2)
lines(x_values,m_hat_2[,3], col = "blue", lwd = 2)
legend("topright",c("locpoly-inbuilt-est","mean_approach_est","median_approach_est"), fill = c("black","red","blue"))

#error
mean_sq_error_mean_apporach <- sum( (y - m_hat_1[,1])^2 )

mean_sq_error_median_apporach <- sum((y - m_hat_2[,1])^2 )


### we can see that mean_sq_error_mean_apporach is less than mean_sq_error_median_apporach
### From above graph mean approach estimator better than median approach because meadian approach estimator is fluctuating much more than mean approach
### mean approach  looks quite good and fit smoothly



