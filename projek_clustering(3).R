install.packages("ggplot2")
library(ggplot2)

#import dataset
Credit.Card.Customer.Data <- read.csv("~/kuliah/ccit/semester 3/DAP/Credit Card Customer Data.csv")
#menghilangkan label nomer
Credit.Card.Customer.Data <- Credit.Card.Customer.Data[-1]

#melihat korelasi
correlation <- cor(Credit.Card.Customer.Data)
print(correlation)

#plot korelasi antar variabel
correlation_matrix <- cor(Credit.Card.Customer.Data)
heatmap_plot <- ggplot(data = reshape2::melt(correlation_matrix), 
                       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8),
        axis.text.y = element_text(angle = 0, vjust = 1, size = 8))
print(heatmap_plot)

#menghitung jumlah nilai kosong pada setiap kolum
sum(is.na(Credit.Card.Customer.Data))

#antara credit limit dan total visits online
#memilih kolum 2 dan 5
x_25 <- Credit.Card.Customer.Data[, c(2, 5)]
print(x_25)
#normalisasi data
x_norm3 <- scale(x_25)
print(x_norm3)

#clustering menggunakan k-means
# Menentukan jumlah cluster
num_clusters3 <- 3
# Melakukan K-Means clustering
kmeans_result3 <- kmeans(x_norm3, centers = num_clusters3)
# Mendapatkan label cluster
labels3 <- kmeans_result3$cluster
# Mendapatkan nilai centroid
centroids3 <- kmeans_result3$centers
print(centroids3)

# Menentukan jumlah cluster dan melakukan K-Means clustering
kmeans_model3 <- kmeans(x_norm3, centers = 3, nstart=10, iter.max=300)
# Mendapatkan hasil clustering
y_kmeans3 <- kmeans_model3$cluster
print(y_kmeans3)

# Menambahkan kolom Cluster ke data frame
dataCluster3 <- y_kmeans3
# Menghitung jumlah anggota masing-masing cluster
cluster_counts3 <- table(dataCluster3)
# Menampilkan jumlah anggota masing-masing cluster
print(cluster_counts3)

# Membuat data frame untuk visualisasi
visualization_data3 <- data.frame(x1 = x[,1], x2 = x[,2], Cluster = as.factor(y_kmeans3))
# Membuat plot scatter dengan ggplot2
scatter_plot3 <- ggplot(visualization_data3, aes(x = x1, y = x2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centroids3), 
             aes(x = Avg_Credit_Limit, y = Total_visits_online), color = 'black', size = 5, shape = 3) +
  labs(title = 'Clusters of Credit Card Customer',
       x = 'Avg_Credit_Limit',
       y = 'Total_visit_online',
       color = 'Cluster') +
  theme_minimal()
print(scatter_plot3)

# Menghitung jumlah anggota masing-masing klaster
cluster_counts3 <- table(y_kmeans3)
# Menampilkan jumlah anggota masing-masing klaster
for (i in seq_along(cluster_counts3)) {
  print(paste("Cluster", i, ":", cluster_counts3[i], "data points"))
}