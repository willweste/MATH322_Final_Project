setwd("~/Documents/JamesMadisonUniversity/Senior/Spring2025/MATH322/MATH322_Final_Project/data")

ames <- read.csv("AmesHousing.csv")

summary(ames)

ames_subset <- na.omit(ames[, c("SalePrice", "Gr.Liv.Area", "Overall.Qual", "Year.Built",
                                "Garage.Cars", "Total.Bsmt.SF", "Neighborhood", "Kitchen.Qual")])

ames_subset$Neighborhood <- as.factor(ames_subset$Neighborhood)
ames_subset$Kitchen.Qual <- as.factor(ames_subset$Kitchen.Qual)

ames_subset$LogSalePrice <- log(ames_subset$SalePrice)

model <- lm(LogSalePrice ~ Gr.Liv.Area + Overall.Qual + Year.Built +
              Garage.Cars + Total.Bsmt.SF + Neighborhood + Kitchen.Qual,
            data = ames_subset)

summary(model)


par(mfrow = c(2, 2))
plot(model)


