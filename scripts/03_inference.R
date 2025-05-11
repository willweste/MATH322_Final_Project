# 03_inference.R
# MATH 322 Final Project - Statistical Inference
# Clear workspace
rm(list=ls())

setwd("/Users/willweste/MATH322/MATH322_Final_Project")

# Read the cleaned dataset
ames_subset <- read.csv("data/ames_clean.csv")

# Ensure categorical variables are treated as factors
ames_subset$Neighborhood <- as.factor(ames_subset$Neighborhood)
ames_subset$Kitchen.Qual <- as.factor(ames_subset$Kitchen.Qual)

# Load the final models
final_model_rq1 <- readRDS("data/final_model_rq1.rds")
interaction_model <- readRDS("data/final_model_rq2.rds")

# RESEARCH QUESTION 1: Important factors affecting home prices

# Extract coefficients and confidence intervals
summary(final_model_rq1)
confint(final_model_rq1, level = 0.95)

# Interpretable effects for numeric predictors
numeric_vars <- c("Gr.Liv.Area", "Overall.Qual", "Year.Built", 
                 "Garage.Cars", "Total.Bsmt.SF")

# Convert log coefficients to percentage effects
for (var in numeric_vars) {
  if (var %in% names(coef(final_model_rq1))) {
    coef_val <- coef(final_model_rq1)[var]
    
    # For continuous predictors, choose appropriate unit change
    if (var == "Gr.Liv.Area") {
      unit_change <- 100  # 100 sq ft
      effect <- (exp(coef_val * unit_change) - 1) * 100
      cat("Effect of +100 sq ft in living area: ", round(effect, 2), "% increase in price\n")
    } else if (var == "Overall.Qual") {
      effect <- (exp(coef_val) - 1) * 100
      cat("Effect of +1 point in overall quality: ", round(effect, 2), "% increase in price\n")
    } else if (var == "Year.Built") {
      unit_change <- 10  # 10 years newer
      effect <- (exp(coef_val * unit_change) - 1) * 100
      cat("Effect of +10 years in home age: ", round(effect, 2), "% increase in price\n")
    } else if (var == "Garage.Cars") {
      effect <- (exp(coef_val) - 1) * 100
      cat("Effect of +1 car in garage capacity: ", round(effect, 2), "% increase in price\n")
    } else if (var == "Total.Bsmt.SF") {
      unit_change <- 100  # 100 sq ft
      effect <- (exp(coef_val * unit_change) - 1) * 100
      cat("Effect of +100 sq ft in basement area: ", round(effect, 2), "% increase in price\n")
    }
  }
}

# RESEARCH QUESTION 2: Interaction between living area and quality

# Extract coefficients from interaction model
summary(interaction_model)

# ANOVA test comparing models with and without interaction
no_interaction_model <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual, 
                          data = ames_subset)
anova(no_interaction_model, interaction_model)

# Effect of living area at different quality levels
coef_main <- coef(interaction_model)["Gr.Liv.Area"]
coef_int <- coef(interaction_model)["Gr.Liv.Area:Overall.Qual"]

# Calculate effect of 100 sq ft increase at different quality levels
quality_levels <- c(3, 5, 7, 9)  # Low, medium-low, medium-high, high

cat("\nEffect of 100 sq ft increase in living area by quality level:\n")
for (qual in quality_levels) {
  total_effect <- coef_main + coef_int * qual
  percent_change <- (exp(total_effect * 100) - 1) * 100  # Effect of 100 sq ft
  cat("Quality level ", qual, ": ", round(percent_change, 2), "% increase in price\n", sep="")
}

# PREDICTION EXAMPLES

# Created function to predict house prices
predict_house_price <- function(living_area, overall_qual, year_built, 
                               garage_cars, total_bsmt_sf, neighborhood, kitchen_qual) {
  
  # Create a new data frame with some input values
  new_house <- data.frame(
    Gr.Liv.Area = living_area,
    Overall.Qual = overall_qual,
    Year.Built = year_built,
    Garage.Cars = garage_cars,
    Total.Bsmt.SF = total_bsmt_sf,
    Neighborhood = factor(neighborhood, levels = levels(ames_subset$Neighborhood)),
    Kitchen.Qual = factor(kitchen_qual, levels = levels(ames_subset$Kitchen.Qual))
  )
  
  # Get prediction in log scale
  log_prediction <- predict(final_model_rq1, newdata = new_house, interval = "prediction")
  
  # Convert to original scale
  price_prediction <- exp(log_prediction)
  
  return(price_prediction)
}

# Another example I thought of: Predict price for a sample house
example_prediction <- predict_house_price(
  living_area = 1800,       # 1800 sq ft living area
  overall_qual = 7,         # Above average quality (7 out of 10)
  year_built = 2000,        # Built in 2000
  garage_cars = 2,          # 2-car garage
  total_bsmt_sf = 1000,     # 1000 sq ft basement
  neighborhood = "NridgHt", # Northridge Heights neighborhood
  kitchen_qual = "Gd"       # Good kitchen quality
)

cat("\nExample prediction:\n")
cat("Estimated sale price: $", round(example_prediction[1], 2), "\n")
cat("95% prediction interval: $", round(example_prediction[2], 2), 
    " to $", round(example_prediction[3], 2), "\n")

# SUMMARY OF FINDINGS

cat("\n=== SUMMARY OF FINDINGS ===\n")

# Research Question 1
cat("\nResearch Question 1: What are the most important factors that influence the sale price of a home in Ames?\n")
cat("\nThe most important factors (based on t-values) are:\n")
model_summary <- summary(final_model_rq1)
coef_table <- model_summary$coefficients
# Get top 5 coefficients by absolute t-value
top_indices <- order(abs(coef_table[, "t value"]), decreasing = TRUE)[1:10]
top_coefs <- coef_table[top_indices, ]
print(top_coefs)

# Research Question 2
cat("\nResearch Question 2: Does the effect of living area on sale price vary depending on the perceived quality of the home?\n")
anova_result <- anova(no_interaction_model, interaction_model)
cat("\nInteraction test result: F =", round(anova_result$F[2], 2), 
    ", p-value =", anova_result$`Pr(>F)`[2], "\n")

if (anova_result$`Pr(>F)`[2] < 0.05) {
  cat("Conclusion: There is a significant interaction effect. The impact of additional square footage\n")
  cat("on home price depends on the overall quality of the home.\n\n")
} else {
  cat("Conclusion: There is no significant interaction effect. The impact of additional square footage\n")
  cat("on home price does not appear to depend on the overall quality of the home.\n")
}