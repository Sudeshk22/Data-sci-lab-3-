
X <-iris[iris$Species == "virginica", c("Petal.Length", "Petal.Width")]
X <- as.matrix(X)
plot(X,xlim = c(4,7),ylim = c(1,4))
d <- 2
n <- 50

tri <- function(X,Q,u,n)
{
  A <- n*u
  for (i in 1:n) 
  {
    A <- A + (X[i,] - Q)/norm(X[i,]-Q, type = c("2"))
  }
  return(A)
}
shi <- function(X,Q,n, d)
{
  A <- rep(0,d)
  for (i in 1:n) {
    A <- A + (diag(1,nrow = d, ncol = d) - (X[i,] - Q)%*%t((X[i,] - Q))/(norm(X[i,]-Q, type = "2"))^2)/(norm(X[i,] - Q,type = "2"))
  }
  return(A)
}



generate_vector <- function(mod_u) {
  # Generate random angle between 0 and 2*pi
  theta <- runif(100, 0, 2 * pi)
  theta <- sort(theta)
  # Calculate x and y components based on the angle
  x <- (mod_u)*cos(theta)
  y <- (mod_u)*sin(theta)
  
  # Return the vector
  A <- cbind(x,y)
  return(A)
}

# Generate 100 vectors
quant_cont <- function(mode_u)
{  
vectors<- generate_vector(mode_u)

quant <- matrix(0,nrow =  100, ncol = 2)
for(i in 1:100)
{
  u <- vectors[i,]
  tol <- 1e-12
  compare <- 100
  itr <- 1
  Q.current <- colMeans(X)
  Q.new <- Q.current
  while (compare > tol && itr < 1e2) {
    itr <- itr + 1
    B <- tri(X,Q.current,u,n)
    C <- shi(X,Q.current,n,d)
    Q.new <- Q.current + solve(C)%*%B
    Q.current <- Q.new
    
    compare <- norm(B, type = "2")
  }
  quant[i,] <- t(Q.new)
}
return(quant)
}

quant1 <- quant_cont(1/10)
quant2 <- quant_cont(2/10)
quant3 <- quant_cont(3/10)
quant4 <- quant_cont(4/10)
quant5 <- quant_cont(5/10)
quant6 <- quant_cont(6/10)
quant7 <- quant_cont(7/10)
quant8 <- quant_cont(8/10)
quant9 <- quant_cont(9/10)


lines(quant1[,1],quant1[,2], type = "l", col = "red")
lines(quant2[,1],quant2[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant3[,1],quant3[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant4[,1],quant4[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant5[,1],quant5[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant6[,1],quant6[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant7[,1],quant7[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant8[,1],quant8[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")
lines(quant9[,1],quant9[,2], type = "l",xlim = c(4,8), ylim = c(1,6), col = "red")

