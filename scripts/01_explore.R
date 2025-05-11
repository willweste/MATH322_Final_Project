# 01_explore.R
# MATH 322 Final Project - Data Exploration
# Clear workspace
rm(list=ls())

# Read data
ames <- read.csv("data/AmesHousing.csv", header = TRUE)

# Basic inspection
str(ames)
dim(ames)

# Summary of key variables
summary(ames[, c("SalePrice", "Gr.Liv.Area", "Overall.Qual", "Year.Built", 
                "Garage.Cars", "Total.Bsmt.SF")])

# Frequency tables for categorical variables
table(ames$Kitchen.Qual)
table(ames$House.Style)
table(ames$Neighborhood)

# Visualizations
# Histogram of Sale Price
options(scipen = 999) 
hist(ames$SalePrice,
     main = "Histogram of Sale Price",
     xlab = "Sale Price (USD)",
     col = "skyblue",
     breaks = 50)

# Histogram of Log Sale Price
hist(log(ames$SalePrice),
     main = "Histogram of Log(Sale Price)",
     xlab = "Log(Sale Price)",
     col = "lightgreen",
     breaks = 50)

# Scatterplot of Living Area vs Sale Price
plot(ames$Gr.Liv.Area, ames$SalePrice,
     main = "Sale Price vs. Living Area",
     xlab = "Above Ground Living Area (sq ft)",
     ylab = "Sale Price (USD)",
     pch = 19,
     col = "blue")

# Scatterplot colored by Overall Quality
plot(ames$Gr.Liv.Area, ames$SalePrice,
     col = as.factor(ames$Overall.Qual),
     pch = 19,
     xlab = "Above Ground Living Area (sq ft)",
     ylab = "Sale Price (USD)",
     main = "Sale Price vs. Living Area Colored by Overall Quality")

legend("topleft", 
       legend = levels(as.factor(ames$Overall.Qual)),
       col = 1:length(levels(as.factor(ames$Overall.Qual))), 
       pch = 19, 
       title = "Overall Quality",
       cex = 0.8)

# Boxplot of Sale Price by Kitchen Quality
boxplot(SalePrice ~ Kitchen.Qual,
        data = ames,
        main = "Sale Price by Kitchen Quality",
        xlab = "Kitchen Quality",
        ylab = "Sale Price (USD)",
        col = "lightpink")

# Correlation matrix
cor_matrix <- cor(ames[, c("SalePrice", "Gr.Liv.Area", "Overall.Qual", "Year.Built", 
                          "Garage.Cars", "Total.Bsmt.SF")], use = "complete.obs")
print(cor_matrix)

# Create dataset for modeling
ames_subset <- na.omit(ames[, c("SalePrice", "Gr.Liv.Area", "Overall.Qual", "Year.Built",
                               "Garage.Cars", "Total.Bsmt.SF", "Neighborhood", "Kitchen.Qual")])

# Convert categorical variables to factors
ames_subset$Neighborhood <- as.factor(ames_subset$Neighborhood)
ames_subset$Kitchen.Qual <- as.factor(ames_subset$Kitchen.Qual)

# Add log-transformed sale price
ames_subset$LogSalePrice <- log(ames_subset$SalePrice)

# Save clean data
write.csv(ames_subset, "data/ames_clean.csv", row.names = FALSE)