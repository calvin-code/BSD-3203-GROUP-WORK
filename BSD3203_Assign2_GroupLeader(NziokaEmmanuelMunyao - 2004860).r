# Install required packages if not already installed
install.packages("dplyr")
install.packages("ggplot2")
install.packages("moments")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(moments)
# Load dataset
sales_data <- read.csv("C:/Users/Nzioka E M/Desktop/sales_data.csv")
# Create the Sales column (Product_Price * Quantity)
sales_data <- sales_data %>% mutate(Sales = Product_Price * Quantity)
# View first few rows
head(sales_data)

# Question 1: Univariate Non-Graphical EDA 
# Measures of central tendency
mean_sales <- mean(sales_data$Sales, na.rm = TRUE)
median_sales <- median(sales_data$Sales, na.rm = TRUE)
mode_sales <- as.numeric(names(sort(table(sales_data$Sales), decreasing = TRUE)[1]))
# Measures of distribution
variance_sales <- var(sales_data$Sales, na.rm = TRUE)
sd_sales <- sd(sales_data$Sales, na.rm = TRUE)
# Skewness and Kurtosis
skewness_sales <- skewness(sales_data$Sales, na.rm = TRUE)
kurtosis_sales <- kurtosis(sales_data$Sales, na.rm = TRUE)
# Print results
cat("Mean:", mean_sales, "\n")
cat("Median:", median_sales, "\n")
cat("Mode:", mode_sales, "\n")
cat("Variance:", variance_sales, "\n")
cat("Standard Deviation:", sd_sales, "\n")
cat("Skewness:", skewness_sales, "\n")
cat("Kurtosis:", kurtosis_sales, "\n")

# Question 2: Univariate Graphical EDA 
# Histogram
ggplot(sales_data, aes(x = Sales)) + 
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Sales", x = "Sales", y = "Frequency")

# Stem-and-leaf plot
stem(sales_data$Sales)

# Boxplot for outliers
ggplot(sales_data, aes(y = Sales)) + 
  geom_boxplot(fill = "red", alpha = 0.6) +
  labs(title = "Boxplot of Sales")

# Q-Q plot
qqnorm(sales_data$Sales)
qqline(sales_data$Sales, col = "red")

# Question 3: Multivariate EDA 
# Cross-tabulation
sales_table <- table(sales_data$Region, sales_data$Product_Category)
print(sales_table)

# Covariance & Correlation
cov_sales_quantity <- cov(sales_data$Sales, sales_data$Quantity, use = "complete.obs")
cor_sales_quantity <- cor(sales_data$Sales, sales_data$Quantity, use = "complete.obs")

cat("Covariance:", cov_sales_quantity, "\n")
cat("Correlation:", cor_sales_quantity, "\n")

# Barplot of total sales per region
ggplot(sales_data, aes(x = Region, y = Sales)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Total Sales per Region", x = "Region", y = "Total Sales")

# Scatterplot of Sales vs Quantity
ggplot(sales_data, aes(x = Quantity, y = Sales)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot of Sales vs Quantity", x = "Quantity", y = "Sales")

# Boxplot of Sales per Product_Category
ggplot(sales_data, aes(x = Product_Category, y = Sales)) +
  geom_boxplot(fill = "purple", alpha = 0.6) +
  labs(title = "Boxplot of Sales per Product Category", x = "Product Category", y = "Sales")
