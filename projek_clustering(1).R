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

#antara credit limit dan total credit card
#memilih kolum 2 dan 3
x_23 <- Credit.Card.Customer.Data[, c(2, 3)]
print(x_23)
#normalisasi data
x_norm1 <- scale(x_23)
print(x_norm1)

#clustering menggunakan k-means
# Menentukan jumlah cluster
num_clusters1 <- 3
# Melakukan K-Means clustering
kmeans_result1 <- kmeans(x_norm1, centers = num_clusters1)
# Mendapatkan label cluster
labels1 <- kmeans_result1$cluster
# Mendapatkan nilai centroid
centroids1 <- kmeans_result1$centers
print(centroids1)

# Menentukan jumlah cluster dan melakukan K-Means clustering
kmeans_model1 <- kmeans(x_norm1, centers = 3, nstart=10, iter.max=300)
# Mendapatkan hasil clustering
y_kmeans1 <- kmeans_model1$cluster
print(y_kmeans1)

# Menambahkan kolom Cluster ke data frame
dataCluster1 <- y_kmeans1
# Menghitung jumlah anggota masing-masing cluster
cluster_counts1 <- table(dataCluster1)
# Menampilkan jumlah anggota masing-masing cluster
print(cluster_counts1)

# Membuat data frame untuk visualisasi
visualization_data1 <- data.frame(x1 = x[,1], x2 = x[,2], Cluster = as.factor(y_kmeans1))
# Membuat plot scatter dengan ggplot2
scatter_plot1 <- ggplot(visualization_data1, aes(x = x1, y = x2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(centroids1), 
             aes(x = Avg_Credit_Limit, y = Total_Credit_Cards), color = 'black', size = 5, shape = 3) +
  labs(title = 'Clusters of Credit Card Customer',
       x = 'Avg_Credit_Limit',
       y = 'Total_Credit_Cards',
       color = 'Cluster') +
  theme_minimal()
print(scatter_plot1)

# Menghitung jumlah anggota masing-masing klaster
cluster_counts1 <- table(y_kmeans1)
# Menampilkan jumlah anggota masing-masing klaster
for (i in seq_along(cluster_counts1)) {
  print(paste("Cluster", i, ":", cluster_counts1[i], "data points"))
}