# Math 322 Final Project

rm(list=ls())

ames<- read.csv(file.choose(), header = TRUE)

attach(ames)

# Introduction Descriptive Statistics
summary(ames[, c("SalePrice", "Gr.Liv.Area", "Overall.Qual", "Year.Built")])
table(House.Style)
table(Kitchen.Qual)

# Introduction Visualizations
# Histogram of SalePrice
options(scipen = 999)  # For x-axis price readability
hist(SalePrice,
     main = "Histogram of Sale Price",
     xlab = "Sale Price (USD)",
     col = "skyblue",
     breaks = 50)

# Boxplots of SalePrice by Categorical Variables
# By Kitchen.Qual
boxplot(SalePrice ~ Kitchen.Qual,
        data = ames,
        main = "Sale Price by Kitchen Quality",
        xlab = "Kitchen Quality",
        ylab = "Sale Price (USD)",
        col = "lightpink")

# By Neighborhood
boxplot(SalePrice ~ Neighborhood,
        data = ames,
        main = "Sale Price by Neighborhood",
        xlab = "Neighborhood",
        ylab = "Sale Price (USD)",
        col = "lightblue",
        las = 2,
        cex.axis = 0.7)

# By House.Style
boxplot(SalePrice ~ House.Style,
        data = ames,
        main = "Sale Price by House Style",
        xlab = "House Style",
        ylab = "Sale Price (USD)",
        col = "lightgray")

# Scatterplot of Gr.Liv.Area vs. SalePrice, Colored by Overall.Qual
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

# Bar Plot of Overall.Qual Counts
barplot(table(ames$Overall.Qual),
        main = "Counts of Overall Quality Ratings",
        xlab = "Overall Quality (1 = Poor, 10 = Excellent)",
        ylab = "Number of Houses",
        col = "orange")
