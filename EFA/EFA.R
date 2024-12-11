data <- read.csv("C:/Users/Baig/Downloads/Stats Project/Data/quarterly_revenue_data.csv")

library(psych)
library(GPArotation)

numeric_data <- data[, c("y", "lag.quarterly.revenue", "price.index", "income.level", "market.potential")]

# Remove rows with missing values
numeric_data <- na.omit(numeric_data)

# View summary statistics
summary(numeric_data)

kmo_result <- KMO(cor(numeric_data))
print(kmo_result)

bartlett_result <- cortest.bartlett(cor(numeric_data), n = nrow(numeric_data))
print(bartlett_result)

# Perform factor analysis with varimax rotation
efa_result <- fa(numeric_data, nfactors = 2, rotate = "varimax")

# Print results
print(efa_result)

# View factor loadings
efa_result$loadings

# Variance explained by factors
efa_result$Vaccounted


