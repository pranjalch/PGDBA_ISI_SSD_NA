# Install Required Packages
if (!require(psych)) install.packages("psych", dependencies = TRUE)
if (!require(pheatmap)) install.packages("pheatmap", dependencies = TRUE)
if (!require(MASS)) install.packages("MASS", dependencies = TRUE)

# Load libraries
library(psych)
library(pheatmap)
library(MASS)

##################################### UNIVARIATE ANALYSIS #####################################

##### 1. Data Overview #####

# Load the Boston Housing dataset
data(Boston)

# Display the structure of the dataset
str(Boston)


# Calculate the number of observations (rows) and variables (columns)
num_observations <- nrow(Boston)
num_variables <- ncol(Boston)
print(paste("Number of observations:", num_observations))
print(paste("Number of variables:", num_variables))


##### 2. Summary Statistics #####

# Numerical variable (age - proportion of owner-occupied units built before 1940)
num_var <- Boston$age

# Calculate summary statistics
mean_value <- mean(num_var)  # Mean
median_value <- median(num_var)  # Median
sd_value <- sd(num_var)  # Standard deviation
min_value <- min(num_var)  # Minimum
max_value <- max(num_var)  # Maximum

# Summary statistics table
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
  Value = c(mean_value, median_value, sd_value, min_value, max_value)
)
print(summary_table)

# View dataframe
num_var_desc <- describe(Boston)
boston_desc <- num_var_desc["age", ]
View(as.data.frame(boston_desc))


##### 3. Distribution Visualization #####

# Histogram
hist(num_var,col='lightblue',main=paste("Histogram of Age of housing stock (age)"),
     xlab="age", ylab="Frequency")

# Boxplot
boxplot(num_var, col='lightblue',outpch=19,outcol='red',
        main=paste("Boxplot of Age of housing stock (age)"), ylab="age")


##### 4. Categorical Variable Analysis #####

# Categorical variable (rad - index of accessibility to radial highways)
cat_var <- Boston$rad

# Create a table of counts for each category in the 'rad' variable
rad_counts <- table(cat_var)

# Bar Plot
barplot(rad_counts, 
        main = "Bar Plot of Radial Highway Accessibility", 
        xlab = "Radial Highway Accessibility (RAD)", 
        ylab = "Count of Tracts", 
        col = "lightblue", 
        border = "black")



##################################### MULTIIVARIATE ANALYSIS #####################################


##### 5. Correlation Analysis #####

# Two numerical variables 'age' and 'dis - weighted distances to five Boston employment centers'
age <- Boston$age
dis <- Boston$dis

# Pearson correlation coefficient
correlation <- cor(age, dis)
print(paste("Pearson correlation coefficient between age and dis:", correlation))

# Correlation matrix & Heat Map for few numerical variables
corr_matrix = cor(Boston[, c("age", "dis")])
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 50,fontsize_col=14,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=14,
         main="Pearson's correlation cofficient")


##### 6. Scatter Plot Visualization #####

# Scatter plot
plot(age, dis, 
     main = "Scatter Plot of Age vs Distance", 
     xlab = "Age of the housing stock (age)", 
     ylab = "Weighted distances to 5 Boston employment centers (dis)", 
     pch = 19,
     col = "blue")

# Fit a linear model and add trend line
lm_fit <- lm(dis ~ age)
abline(lm_fit, col = "red", lwd = 2)


##### 7. Multiple Regression #####

# Fit the linear regression model 

model <- lm(medv ~ rm + lstat + ptratio + dis, data = Boston)
# model <- lm(medv ~ age + dis + ptratio, data = Boston)

# Model summary
summary(model)

# Predicted values
predicted <- predict(model)

# Actual vs Predicted plot
plot(Boston$medv, predicted, 
     xlab = "Actual MEDV", 
     ylab = "Predicted MEDV", 
     main = "Actual vs Predicted Median Value of Homes (MEDV))", 
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Add a 45-degree reference line


##### 8. Model Diagnostics #####

# Generate diagnostic plots
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)
par(mfrow=c(1,1)) # Reset 

# Residuals vs Fitted Plot
plot(model$fitted.values, residuals(model), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Horizontal reference line


# Q-Q Plot (Check for Normality)
qqnorm(residuals(model), 
       main = "Normal Q-Q Plot", 
       pch = 19, col = "blue")
qqline(residuals(model), col = "red", lwd = 2)  # Add Q-Q reference line


# Shapiro-Wilk test for normality
shapiro.test(residuals(model))  



##################################### ADVANCED ANALYSIS #####################################

##### 9. Principal Component Analysis (PCA) #####

# Select numerical variables from the dataset
numerical_data <- Boston[, sapply(Boston, is.numeric)]

# Standardize the data
scaled_data <- scale(numerical_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Summary of PCA to see explained variance
pca_summary <- summary(pca_result)
print(pca_summary)

# Extract explained variance
explained_variance <- pca_summary$importance[2, ]  # Proportion of Variance
cumulative_variance <- pca_summary$importance[3, ]  # Cumulative Proportion

# Barplot for explained variance
barplot_heights <- barplot(explained_variance, 
                           main = "Scree Plot with bars for explained variance", 
                           xlab = "Principal Components", 
                           ylab = "Proportion of Variance Explained", 
                           col = "skyblue", 
                           ylim = c(0, 1), 
                           names.arg = paste0("PC", 1:length(explained_variance)))

# Add points and lines for the scree plot
points(barplot_heights, explained_variance, type = "b", 
       col = "red", pch = 19, lwd = 2)


# Plot cumulative explained variance
plot(cumulative_variance, type = "b", 
     main = "Cumulative Explained Variance", 
     xlab = "Principal Components", 
     ylab = "Cumulative Proportion of Variance Explained", 
     pch = 19, col = "blue", ylim = c(0, 1))
abline(h = 0.8, col = "red", lty = 2)  # Reference line for 80% cumulative variance


##### 10. Principal Component Analysis (PCA) #####

# Loadings (contribution of each variable to the PCs)
loadings <- pca_result$rotation[, 1:2]
print(loadings)

# Biplot of PC1 and PC2
biplot(pca_result, main="PCA Biplot", cex=0.5)

# Create a biplot
biplot(pca_result, scale = 0, 
       main = "Biplot of PCA Results", 
       col = c("lightblue", "red"))

# Success message
print("CODE RUN SUCCESSFULL!")

