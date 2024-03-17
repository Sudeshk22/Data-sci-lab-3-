library(readxl)
cancer_data <- read_excel("C:/Users/Pravin Bharti/OneDrive/Desktop/Cancer_data.xlsx")
dim(cancer_data)
colnames(cancer_data)
cancer_data <- cancer_data[, -c(1)]
#################################################
subset_b <- cancer_data[cancer_data$diagnosis == "B", ]
subset_b <- subset_b[,-1]
subset_B <- subset_b[,-1]
dim(subset_B)
#######################################################
subset_m<- cancer_data[cancer_data$diagnosis == "M", ]
subset_m <- subset_m[,-1]
subset_M <- subset_m[,-1]
dim(subset_M)
#################################
cancer <- cancer_data[,-1]
cancer <- cancer[,-1]
colnames(cancer) <- paste0("X", 1:19)
print(cancer)
colnames(subset_M) <- paste0("X", 1:19)
colnames(subset_B) <- paste0("X", 1:19)
subset_B <- data.frame(subset_B)
subset_M <- data.frame(subset_M)
cancer <- data.frame(cancer)

#########################################
joint_ecdf <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                       X11, X12, X13, X14, X15, X16, X17, X18, X19) {
  
  n <- nrow(cancer)
  sum((cancer$X1 <= X1) & (cancer$X2 <= X2) & (cancer$X3 <= X3) &
        (cancer$X4 <= X4) & (cancer$X5 <= X5) & (cancer$X6 <= X6) &
        (cancer$X7 <= X7) & (cancer$X8 <= X8) & (cancer$X9 <= X9) &
        (cancer$X10 <= X10) & (cancer$X11 <= X11) & (cancer$X12 <= X12) &
        (cancer$X13 <= X13) & (cancer$X14 <= X14) & (cancer$X15 <= X15) &
        (cancer$X16 <= X16) & (cancer$X17 <= X17) & (cancer$X18 <= X18) &
        (cancer$X19 <= X19)) / n
}
################################################################
Subset_B_ecdf <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                       X11, X12, X13, X14, X15, X16, X17, X18, X19) {
  
  n_1 <- nrow(subset_B)
  sum((subset_B$X1 <= X1) & (subset_B$X2 <= X2) & (subset_B$X3 <= X3) &
        (subset_B$X4 <= X4) & (subset_B$X5 <= X5) & (subset_B$X6 <= X6) &
        (subset_B$X7 <= X7) & (subset_B$X8 <= X8) & (subset_B$X9 <= X9) &
        (subset_B$X10 <= X10) & (subset_B$X11 <= X11) & (subset_B$X12 <= X12) &
        (subset_B$X13 <= X13) & (subset_B$X14 <= X14) & (subset_B$X15 <= X15) &
        (subset_B$X16 <= X16) & (subset_B$X17 <= X17) & (subset_B$X18 <= X18) &
        (subset_B$X19 <= X19)) / n_1
}
################################################
Subset_M_ecdf <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,
                          X11, X12, X13, X14, X15, X16, X17, X18, X19) {
  
  n_2 <- ncol(subset_M)
  sum((subset_M$X1 <= X1) & (subset_M$X2 <= X2) & (subset_M$X3 <= X3) &
        (subset_M$X4 <= X4) & (subset_M$X5 <= X5) & (subset_M$X6 <= X6) &
        (subset_M$X7 <= X7) & (subset_M$X8 <= X8) & (subset_M$X9 <= X9) &
        (subset_M$X10 <= X10) & (subset_M$X11 <= X11) & (subset_M$X12 <= X12) &
        (subset_M$X13 <= X13) & (subset_M$X14 <= X14) & (subset_M$X15 <= X15) &
        (subset_M$X16 <= X16) & (subset_M$X17 <= X17) & (subset_M$X18 <= X18) &
        (subset_M$X19 <= X19)) / n_2
}
#######################################
num_points <- 2

# Create grid values
grid_values <- do.call(expand.grid, lapply(1:19, function(i) seq(min(cancer[[paste0("X", i)]]), max(cancer[[paste0("X", i)]]), length.out = num_points)))
###########################################
# Calculate joint ECDF values for each point in the grid
ecdf_values_joint <- apply(grid_values, 1, function(row) joint_ecdf(row[1], row[2], row[3], row[4], row[5], row[6], row[7], row[8], row[9], row[10], row[11], row[12], row[13], row[14], row[15], row[16], row[17], row[18], row[19]))
summary(ecdf_values_joint)
# Calculate marginal ECDF values for group 1
ecdf_values_group1 <- apply(grid_values, 1, function(row) Subset_B_ecdf(row[1], row[2], row[3], row[4], row[5], row[6], row[7], row[8], row[9], row[10], row[11], row[12], row[13], row[14], row[15], row[16], row[17], row[18], row[19]))
summary(ecdf_values_group1)
# Calculate marginal ECDF values for group 2
ecdf_values_group2 <- apply(grid_values, 1, function(row) Subset_M_ecdf(row[1], row[2], row[3], row[4], row[5], row[6], row[7], row[8], row[9], row[10], row[11], row[12], row[13], row[14], row[15], row[16], row[17], row[18], row[19]))
summary(ecdf_values_group2)
# Calculate the absolute distance between joint and the product of marginals
distance_values <- abs(ecdf_values_joint - (ecdf_values_group1 * ecdf_values_group2))

# Find the supremum (maximum) of the absolute distance
supremum_distance <- max(distance_values)

#######################################################



