#### problem 2
set.seed(2)
m_x <- function(ceff, t)
{
  a <- ceff[1] *sin(2*pi*t)
  
  return(a)
}
t <- seq(0,1,length = 10000)
n <- 30 # no. of X in L2[0,1] space
val <- runif(n, min = 4, max = 7)
ceff <- matrix(val, nrow = n, ncol = 1)

f <- apply(ceff,1,m_x,t)
plot(t,f[,1],type = "l", ylim = c(-10,8))
for (i in 2:n) {
  lines(t,f[,i], col = i)
}

m_xsq <- function(ceff, t,i){
  a <- ceff[i] *sin(2*pi*t)
  
  return((a)^2)
}

y <- numeric(length = n)
for(i in 1:n)
{
  y[i] <- integrate(m_xsq,lower = 0, upper = 1,ceff = ceff, i = i)$value + rnorm(1)
}


## kernel
K <- function(h,u)
{
  d <- dnorm(u/h)
  return(d)
}
# Define the function d
d <- function(t, ceff, i, j) {
  a <- ceff[i] * sin(2*pi*t)
  b <- ceff[j] * sin(2*pi*t)
  c <- (a - b)^2
  return(c)
}


h <- 0.0001
m_hat <- function(modval,y,h)
{
  low <- 0
  upp <- 0
  for (i in 1:n) {
    low <- low + K(h,modval[i])
    upp <- upp + K(h, modval[i])*y[i]
  }
  return(upp/low)
}





foo <- function(i)
{
  modval <- numeric(length = n)
  for (j in 1:n) {
    modval[j] <- integrate(d, lower = 0, upper = 1, ceff = ceff, i = i, j = j)$value
  }
  modval <- sqrt(modval)
  return(modval)
}


est_y <- numeric(length = n)
for (i in 1:n) {
  modval <- foo(i)
  est_y[i] <- m_hat(modval, y, h)
}
data <- data.frame(true_y = y, est_y = est_y)

print(h)
print(data)
mse <- sum((y-est_y)^2)


### mse
h <- seq(0.0001,1,length = 10)
mse <- numeric(length = 10)
for (j in 1:length(h)) {
  for (i in 1:20) {
    modval <- foo(i)
    est_y[i] <- m_hat(modval, y, h = h[j])
  }
  mse[j] <- sum( (y - est_y)^2 )
}
plot(h,mse, type= "l")

