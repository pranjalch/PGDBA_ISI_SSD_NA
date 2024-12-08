# Install Required Packages
if (!require(psych)) install.packages("psych", dependencies = TRUE)
if (!require(pheatmap)) install.packages("pheatmap", dependencies = TRUE)
if (!require(MASS)) install.packages("MASS", dependencies = TRUE)
if (!require(nycflights13)) install.packages("nycflights13", dependencies = TRUE)

# Load libraries
library(psych)
library(pheatmap)
library(MASS)
library(nycflights13)

##################################### UNIVARIATE ANALYSIS #####################################

##### 1. Data Overview #####

# Load the NYC Flights dataset
data(flights)

# DATA CLEANING (Pre-processing)  !!! NOTE - VERY LARGE DATASET !!!

# Set a seed for reproducibility
set.seed(123)

# Find number of NA values in each column
col_na_count <- colSums(is.na(flights))
col_na_count

# Drop rows with any NA values
flights_omit <- na.omit(flights)

# Randomly delete 90% rows to speed up analysis (original data has > 1 lakh rows)
num_rows_to_remove <- floor(nrow(flights_omit) * 0.90)
rows_to_remove <- sample(1:nrow(flights_omit), num_rows_to_remove)
flights_clean <- flights_omit[-rows_to_remove, ]

# Display the structure of the dataset
str(flights_clean)

# Calculate the number of observations (rows) and variables (columns)
num_observations <- nrow(flights_clean)
num_variables <- ncol(flights_clean)
print(paste("Number of observations:", num_observations))
print(paste("Number of variables:", num_variables))


##### 2. Summary Statistics #####

# Numerical variable (air_time - The air time in minutes)
num_var <- flights_clean$air_time

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
num_var_desc <- describe(flights_clean)
flights_desc <- num_var_desc["air_time", ]
View(as.data.frame(flights_desc))


##### 3. Distribution Visualization #####

# Histogram
hist(num_var,col='lightblue',main=paste("Histogram of air_time"),
     xlab="air_time (minutes)", ylab="Frequency")

# Boxplot
boxplot(num_var, col='lightblue',outpch=19,outcol='red',
        main=paste("Boxplot of air_time"), ylab="air_time (minutes)")


##### 4. Categorical Variable Analysis #####

# Categorical variable (carrier)
cat_var <- flights_clean$carrier

# Create a table of counts for each category in the 'gear' variable
carrier_counts <- table(cat_var)

# Bar Plot
barplot(carrier_counts, 
        main = "Bar Plot of Airline Carrier", 
        xlab = "Airline Carrier", 
        ylab = "Count of Flights", 
        col = "lightblue", 
        border = "black")



##################################### MULTIIVARIATE ANALYSIS #####################################


##### 5. Correlation Analysis #####

# Two numerical variables 'air_time' and 'distance'
air_time <- flights_clean$air_time
distance <- flights_clean$distance


# Pearson correlation coefficient
correlation <- cor(air_time, distance)
print(paste("Pearson correlation coefficient between age and dis:", correlation))

# Correlation matrix & Heat Map for few numerical variables
corr_matrix = cor(flights_clean[,  c("air_time", "distance")])
pheatmap(corr_matrix,display_numbers=TRUE,fontsize_number = 50,fontsize_col=14,  
         legend = TRUE, cluster_rows = FALSE,cluster_cols = FALSE,fontsize_row=14,
         main="Pearson's correlation cofficient")


##### 6. Scatter Plot Visualization #####

# Scatter plot
plot(air_time, distance, 
     main = "Scatter Plot of Air time vs Distance", 
     xlab = "Air time (minutes))", 
     ylab = "Distance (miles)", 
     pch = 19,
     col = "blue")
# Fit a linear model and add trend line
lm_fit <- lm(distance ~ air_time)
abline(lm_fit, col = "red", lwd = 2)



##### 7. Multiple Regression #####

# Fit the linear regression model 
model <- lm(arr_delay ~ dep_delay + air_time + distance, data = flights_clean)

# Model summary
summary(model)

# Predicted values
predicted <- predict(model)

# Actual vs Predicted plot
plot(flights_clean$arr_delay, predicted, 
     xlab = "Actual Arrival Delay", 
     ylab = "Predicted Arrival Delay", 
     main = "Actual vs Predicted Arrival Delay", 
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
# Cannot be performed since "sample size must be between 3 and 5000"
# shapiro.test(residuals(model))  



##################################### ADVANCED ANALYSIS #####################################

##### 9. Principal Component Analysis (PCA) #####

# Select numerical variables from the dataset
numerical_data <- flights_clean[, sapply(flights_clean, is.numeric)]

# Drop flight (flight number) column
numerical_data <- numerical_data[, !colnames(numerical_data) %in% "flight"]


# Check variance of each column to identify constant columns
variances <- apply(numerical_data, 2, var)
print(variances)

# Remove columns with zero variance
numerical_data <- numerical_data[, variances != 0]

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
# biplot(pca_result, main="PCA Biplot", cex=0.5)

# Create a biplot
biplot(pca_result, scale = 0, 
       main = "Biplot of PCA Results", 
       col = c("lightblue", "red"))


# Success message
print("CODE RUN SUCCESSFULL!")
print("Please wait for plots to load. Very large dataset!")

