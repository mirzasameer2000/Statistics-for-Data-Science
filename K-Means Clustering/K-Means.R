data <- read.csv("C:/Users/Baig/Downloads/Stats Project/Data/treatment_data.csv")

library(tidyverse)
library(cluster)
library(factoextra)

# Select only numeric columns for clustering
numeric_data <- data %>% select(decrease, rowpos, colpos)

# Scale the data (important for K-Means)
scaled_data <- scale(numeric_data)

# Compute within-cluster sum of squares for different cluster numbers
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Apply K-Means clustering with the chosen number of clusters
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)  # Replace 3 with 4 if needed

# Add cluster assignments to the original dataset
data$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters in a 2D plot
fviz_cluster(kmeans_result, data = scaled_data, geom = "point", ellipse.type = "norm")

kmeans_result$centers

kmeans_result$size

silhouette_score <- silhouette(kmeans_result$cluster, dist(scaled_data))
summary(silhouette_score)

# Generate silhouette plot
silhouette_plot <- silhouette(kmeans_result$cluster, dist(scaled_data))
plot(silhouette_plot, main = "Silhouette Plot for K-Means Clustering")

library(scatterplot3d)
# 3D scatter plot of clusters
scatterplot3d(data$decrease, data$rowpos, data$colpos, color = kmeans_result$cluster,
              pch = 16, main = "3D Scatter Plot of Clusters")



