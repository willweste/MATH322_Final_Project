# 02_modeling.R
# MATH 322 Final Project - Model Building and Diagnostics
# Clear workspace
rm(list=ls())

setwd("/Users/willweste/MATH322/MATH322_Final_Project")

# Read the cleaned dataset
ames_subset <- read.csv("data/ames_clean.csv")

# Ensure categorical variables are treated as factors
ames_subset$Neighborhood <- as.factor(ames_subset$Neighborhood)
ames_subset$Kitchen.Qual <- as.factor(ames_subset$Kitchen.Qual)

# RESEARCH QUESTION 1: Important factors affecting home prices

# Build the full model with all predictors
full_model <- lm(LogSalePrice ~ Gr.Liv.Area + Overall.Qual + Year.Built +
                  Garage.Cars + Total.Bsmt.SF + Neighborhood + Kitchen.Qual,
                data = ames_subset)

# Model summary
summary(full_model)

# Stepwise selection using AIC
step_model <- step(full_model, direction = "both")
summary(step_model)

# Final model for RQ1
final_model_rq1 <- step_model

# RESEARCH QUESTION 2: Interaction between living area and quality

# Build interaction model
interaction_model <- lm(log(SalePrice) ~ Gr.Liv.Area * Overall.Qual, 
                       data = ames_subset)

# Model summary
summary(interaction_model)

# Compare with model without interaction
no_interaction_model <- lm(log(SalePrice) ~ Gr.Liv.Area + Overall.Qual, 
                          data = ames_subset)

# ANOVA test to compare models
anova(no_interaction_model, interaction_model)

# MODEL DIAGNOSTICS

# Diagnostics for RQ1 model
par(mfrow = c(2, 2))
plot(final_model_rq1, main = "Diagnostics for Research Question 1 Model")

# Reset plot parameters
par(mfrow = c(1, 1))

# Test for normality of residuals
shapiro.test(residuals(final_model_rq1))

# Diagnostics for interaction model (RQ2)
par(mfrow = c(2, 2))
plot(interaction_model, main = "Diagnostics for Research Question 2 Model")

# Save models for later use
saveRDS(final_model_rq1, "data/final_model_rq1.rds")
saveRDS(interaction_model, "data/final_model_rq2.rds")