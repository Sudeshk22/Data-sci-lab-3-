# Load the Iris dataset
data(iris)
library(ddalpha)
# Extract data for each species
setosa_data <- iris[iris$Species == "setosa", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
virginica_data <- iris[iris$Species == "virginica", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
versicolor_data <- iris[iris$Species == "versicolor", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
# for centering the data
mean1 <- colMeans(setosa_data)
mean2 <- colMeans(virginica_data)
mean3 <- colMeans(versicolor_data)
for (i in 1:50) {
  setosa_data[i,] <- setosa_data[i,]-mean1
}

for (i in 1:50) {
  virginica_data[i,] <- virginica_data[i,]-mean2
}

for (i in 1:50) {
  versicolor_data[i,] <- versicolor_data[i,] - mean3
}

# Combine data from setosa, virginica and half-space value
combined_data <- rbind( setosa_data,virginica_data)
depthvalue_w.r.t.setosa <- depth.halfspace(combined_data,setosa_data)
depthvalue_w.r.t.virginica <- depth.halfspace(combined_data,virginica_data)

plot(depthvalue_w.r.t.setosa,depthvalue_w.r.t.virginica, pch = 3 )
abline(a = 0, b = 1, col = "red")


#combind data setosa and versicolor and half-space value
combind_se_ver <- rbind(setosa_data,versicolor_data)
depthvalue_se_ver_w.r.t.setosa <- depth.halfspace(combind_se_ver,setosa_data)
depthvalue_se_ver_w.r.t.versicolor <- depth.halfspace(combind_se_ver, versicolor_data)
plot(depthvalue_se_ver_w.r.t.setosa, depthvalue_se_ver_w.r.t.versicolor, pch = 3)
abline( a= 0,  b= 1 , col = "red")

#combind data virginca and versicolor and half-space value
combind_vir_ver <- rbind(virginica_data,versicolor_data)
depthvalue_vir_ver_w.r.t.virginca <- depth.halfspace(combind_vir_ver,virginica_data)
depthvalue_vir_ver_w.r.t.versicolor <- depth.halfspace(combind_vir_ver, versicolor_data)
plot(depthvalue_vir_ver_w.r.t.virginca, depthvalue_vir_ver_w.r.t.versicolor, pch = 3)
abline( a= 0,  b= 1 , col = "red")
