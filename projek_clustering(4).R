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

#antara credit limit dan total calls made
#memilih kolum 2 dan 6
x_26 <- Credit.Card.Customer.Data[, c(2, 6)]
print(x_26)
#normalisasi data
x_norm4 <- scale(x_26)
print(x_norm4)

#clustering menggunakan k-means
# Menentukan jumlah cluster
num_clusters4 <- 3
# Melakukan K-Means clustering
kmeans_result4 <- kmeans(x_norm4, centers = num_clusters4)
# Mendapatkan label cluster
labels4 <- kmeans_result4$cluster
# Mendapatkan nilai centroid
centroids4 <- kmeans_result4$centers
print(centroids4)

# Menentukan jumlah cluster dan melakukan K-Means clustering
kmeans_model4 <- kmeans(x_norm4, centers = 3, nstart=10, iter.max=300)
# Mendapatkan hasil clustering
y_kmeans4 <- kmeans_model4$cluster
print(y_kmeans4)

# Menambahkan kolom Cluster ke data frame
dataCluster4 <- y_kmeans4
# Menghitung jumlah anggota masing-masing cluster
cluster_counts4 <- table(dataCluster4)
# Menampilkan jumlah anggota masing-masing cluster
print(cluster_counts4)

# Membuat data frame untuk visualisasi
visualization_data4 <- data.frame(x1 = x[,1], x2 = x[,2], Cluster = as.factor(y_kmeans4))
# Membuat plot scatter dengan ggplot2
scatter_plot4 <- ggplot(visualization_data4, aes(x = x1, y = x2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centroids4), 
             aes(x = Avg_Credit_Limit, y = Total_calls_made), color = 'black', size = 5, shape = 3) +
  labs(title = 'Clusters of Credit Card Customer',
       x = 'Avg_Credit_Limit',
       y = 'Total_calls_made',
       color = 'Cluster') +
  theme_minimal()
print(scatter_plot4)

# Menghitung jumlah anggota masing-masing klaster
cluster_counts4 <- table(y_kmeans4)
# Menampilkan jumlah anggota masing-masing klaster
for (i in seq_along(cluster_counts4)) {
  print(paste("Cluster", i, ":", cluster_counts4[i], "data points"))
}