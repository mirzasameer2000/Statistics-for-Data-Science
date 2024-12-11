data <- read.csv("C:/Users/Baig/Downloads/Stats Project/Data/DataVisualizationData.csv")

numeric_data <- data[, c("Points", "Goals", "Assists", "Minutes.Played", "Matches.Played")]

# Calculate variance of each column
apply(numeric_data, 2, var)

# PCA without scaling
pca_no_scale <- princomp(numeric_data)

summary(pca_no_scale, loadings = TRUE)

data_cov <- cov(numeric_data)
data_corr <- cor(numeric_data)

pca_cov <- princomp(covmat = data_cov)

pca_corr <- princomp(covmat = data_corr)

summary(pca_cov, loadings = TRUE)

summary(pca_corr, loadings = TRUE)

#correlation matrix PCA is better.

plot(pca_corr$sdev^2, xlab = "Component number", ylab = "Component variance", type = "b", main = "Scree diagram")


plot(log(pca_corr$sdev^2), xlab = "Component number", ylab = "log(Component variance)", type = "b", main = "Log(Eigenvalue) Diagram")


kaizer_rule <- pca_corr$sdev^2 >= 1
print(kaizer_rule)

modified_kaizer_rule <- pca_corr$sdev^2 >= 0.7
print(modified_kaizer_rule)

library(FactoMineR)
library(factoextra)

pca_result <- PCA(numeric_data, graph = TRUE, scale.unit = TRUE)

fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
