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

#antara credit limit dan total visits bank
#memilih kolum 2 dan 4
x_24 <- Credit.Card.Customer.Data[, c(2, 4)]
print(x_24)
#normalisasi data
x_norm2 <- scale(x_24)
print(x_norm2)

#clustering menggunakan k-means
# Menentukan jumlah cluster
num_clusters2 <- 3
# Melakukan K-Means clustering
kmeans_result2 <- kmeans(x_norm2, centers = num_clusters2)
# Mendapatkan label cluster
labels2 <- kmeans_result2$cluster
# Mendapatkan nilai centroid
centroids2 <- kmeans_result2$centers
print(centroids2)

# Menentukan jumlah cluster dan melakukan K-Means clustering
kmeans_model2 <- kmeans(x_norm2, centers = 3, nstart=10, iter.max=300)
# Mendapatkan hasil clustering
y_kmeans2 <- kmeans_model2$cluster
print(y_kmeans2)

# Menambahkan kolom Cluster ke data frame
dataCluster2 <- y_kmeans2
# Menghitung jumlah anggota masing-masing cluster
cluster_counts2 <- table(dataCluster2)
# Menampilkan jumlah anggota masing-masing cluster
print(cluster_counts2)

# Membuat data frame untuk visualisasi
visualization_data2 <- data.frame(x1 = x[,1], x2 = x[,2], Cluster = as.factor(y_kmeans2))
# Membuat plot scatter dengan ggplot2
scatter_plot2 <- ggplot(visualization_data2, aes(x = x1, y = x2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centroids2), 
             aes(x = Avg_Credit_Limit, y = Total_visits_bank), color = 'black', size = 5, shape = 3) +
  labs(title = 'Clusters of Credit Card Customer',
       x = 'Avg_Credit_Limit',
       y = 'Total_visit_bank',
       color = 'Cluster') +
  theme_minimal()
print(scatter_plot2)

# Menghitung jumlah anggota masing-masing klaster
cluster_counts2 <- table(y_kmeans2)
# Menampilkan jumlah anggota masing-masing klaster
for (i in seq_along(cluster_counts2)) {
  print(paste("Cluster", i, ":", cluster_counts2[i], "data points"))
}