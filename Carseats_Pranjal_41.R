# Install Required Packages
if (!require(psych)) install.packages("psych", dependencies = TRUE)
if (!require(pheatmap)) install.packages("pheatmap", dependencies = TRUE)
if (!require(MASS)) install.packages("MASS", dependencies = TRUE)
if (!require(ISLR2)) install.packages("ISLR2", dependencies = TRUE)

# Load libraries
library(psych)
library(pheatmap)
library(MASS)
library(ISLR2)


##################################### UNIVARIATE ANALYSIS #####################################

##### 1. Data Overview #####

# Load the mtcars dataset
data("Carseats")

# Display the structure of the dataset
str(Carseats)

# Calculate the number of observations (rows) and variables (columns)
num_observations <- nrow(Carseats)
num_variables <- ncol(Carseats)
print(paste("Number of observations:", num_observations))
print(paste("Number of variables:", num_variables))


##### 2. Summary Statistics #####

# Numerical variable (hp)
num_var <- Carseats$Price

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
num_var_desc <- describe(Carseats)
price_desc <- num_var_desc["Price", ]
View(as.data.frame(price_desc))


##### 3. Distribution Visualization #####

# Histogram
hist(num_var,col='lightblue',main=paste("Histogram of Price"),
     xlab="Price", ylab="Frequency")

# Boxplot
boxplot(num_var, col='lightblue',outpch=19,outcol='red',
        main=paste("Boxplot of Price"), ylab="Price")


##### 4. Categorical Variable Analysis #####

# Categorical variable (ShelveLoc - Shelving location of the store's car seats)
cat_var <- Carseats$ShelveLoc

# Create a table of counts for each category in the 'ShelveLov' variable
shelveloc_counts <- table(cat_var)

# Bar Plot
barplot(shelveloc_counts, 
        col = c("lightpink", "lightgreen", "lightblue"),
        main = "Bar Plot of Shelving Location", 
        xlab = "Shelving Location", 
        ylab = "Count of Car Seats",
        border = "black")



##################################### MULTIIVARIATE ANALYSIS #####################################


##### 5. Correlation Analysis #####

# Two numerical variables 'Price' and 'Sales'
sales <- Carseats$Sales
price <-  Carseats$Price

# Pearson correlation coefficient
correlation <- cor(price, sales)
print(paste("Pearson correlation coefficient between Price and Sales:", correlation))

# Correlation matrix & Heat Map for all numerical variables
corr_matrix = cor(Carseats[, c("Sales", "Price")])
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 50,fontsize_col=14,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=14,
         main="Pearson's correlation cofficient")


##### 6. Scatter Plot Visualization #####

# Scatter plot
plot(sales, price, 
     main = "Scatter Plot of Sales vs Price", 
     xlab = "Sales", 
     ylab = "Price", 
     pch = 19,
     col = "blue")

# Fit a linear regression model and add trend line
model <- lm(price ~ sales)
abline(model, col = "red", lwd = 2)



##### 7. Multiple Regression #####

# Fit the linear regression model
# Predicting mpg using hp and wt
model <- lm(Sales ~ Price + CompPrice + Advertising, data = Carseats)

# Model summary
summary(model)

# Predicted values
predicted <- predict(model)

# Actual vs Predicted plot
plot(Carseats$Sales, predicted, 
     xlab = "Actual Sales", 
     ylab = "Predicted Sales", 
     main = "Actual vs Predicted Sales", 
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
numerical_data <- Carseats[, sapply(Carseats, is.numeric)]

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

