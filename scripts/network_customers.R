library(tidyverse)
library(dplyr)
library(igraph)
library(ggraph)

whole_df <- read.csv('data_preproc/whole_df.csv', sep = ',')

# S1 ----

s1 <- whole_df %>% filter(shop_id == 'S1')

set.seed(793)
sampled_customers1 <- s1 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s1 <- s1 %>%
  filter(customer_id %in% sampled_customers1$customer_id)

customer_products1 <- aggregate(product_id ~ customer_id, sampled_s1, function(x) unique(x))

clust_s1 <- customer_products1 %>%
  separate_rows(product_id, sep = ",") %>%
  group_by(customer_id) %>%
  mutate(cluster = rep(1:ceiling(n()/100), each = 100, length.out = n())) %>%
  ungroup() %>%
  select(customer_id, cluster) %>%
  distinct() %>%
  mutate(customer_id = paste0(customer_id, "_", cluster)) %>%
  arrange(customer_id)  # Optional: arrange by customer_id for better readability

# View the new dataframe
print(clust_s1)

unique_customers <- unique(clust_s1 $customer_id)
unique_categories <- unique(clust_s1 $cluster)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(clust_s1 )) {
  customer1 <- clust_s1 $customer_id[i]
  category <- clust_s1 $cluster[i]
  
  # Find other customers with the same category
  other_customers <- clust_s1 $customer_id[clust_s1 $cluster == category & 
                                               clust_s1 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set diversity as node sizes
V(g)$colour <- as.factor (clust_s1$cluster)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

palette <- c25[1:num_clusters]

# Define colors for each cluster using the selected palette
cluster_colors <- palette[1:num_clusters]

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(color = as.factor(clust_s1$cluster)), alpha = 0.8, size = 0.5) +  # Assigning colors based on cluster
  scale_color_manual(values = cluster_colors) +  # Applying defined colors
  theme_void() +
  guides(color = "none")

# S2 ----
s2 <- whole_df %>% filter(shop_id == 'S2')
set.seed(793)
sampled_customers2 <- s2 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s2 <- s2 %>%
  filter(customer_id %in% sampled_customers2$customer_id)

customer_products2 <- aggregate(product_id ~ customer_id, sampled_s2, function(x) unique(x))

clust_s2 <- customer_products2 %>%
  separate_rows(product_id, sep = ",") %>%
  group_by(customer_id) %>%
  mutate(cluster = rep(1:ceiling(n()/100), each = 100, length.out = n())) %>%
  ungroup() %>%
  select(customer_id, cluster) %>%
  distinct() %>%
  mutate(customer_id = paste0(customer_id, "_", cluster)) %>%
  arrange(customer_id)  # Optional: arrange by customer_id for better readability

# View the new dataframe
print(clust_s2)

unique_customers <- unique(clust_s2 $customer_id)
unique_categories <- unique(clust_s2 $cluster)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(clust_s2 )) {
  customer1 <- clust_s2 $customer_id[i]
  category <- clust_s2 $cluster[i]
  
  # Find other customers with the same category
  other_customers <- clust_s2 $customer_id[clust_s2 $cluster == category & 
                                             clust_s2 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set diversity as node sizes
V(g)$colour <- as.factor (clust_s2$cluster)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(alpha = 0.5, size = 0.5) +
  theme_void()+
  guides(color = none) 

# S3---- 
s3 <- whole_df %>% filter(shop_id == 'S3')
set.seed(793)
sampled_customers3 <- s3 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s3 <- s3 %>%
  filter(customer_id %in% sampled_customers3$customer_id)

customer_products3 <- aggregate(product_id ~ customer_id, sampled_s3, function(x) unique(x))

clust_s3 <- customer_products3 %>%
  separate_rows(product_id, sep = ",") %>%
  group_by(customer_id) %>%
  mutate(cluster = rep(1:ceiling(n()/100), each = 100, length.out = n())) %>%
  ungroup() %>%
  select(customer_id, cluster) %>%
  distinct() %>%
  mutate(customer_id = paste0(customer_id, "_", cluster)) %>%
  arrange(customer_id)  # Optional: arrange by customer_id for better readability

# View the new dataframe
print(clust_s3)

unique_customers <- unique(clust_s3 $customer_id)
unique_categories <- unique(clust_s3 $cluster)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(clust_s3 )) {
  customer1 <- clust_s3 $customer_id[i]
  category <- clust_s3 $cluster[i]
  
  # Find other customers with the same category
  other_customers <- clust_s3 $customer_id[clust_s3 $cluster == category & 
                                             clust_s3 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set diversity as node sizes
V(g)$colour <- as.factor (clust_s3$cluster)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(alpha = 0.5, size = 0.5) +
  theme_void()

# S4 ----

s4 <- whole_df %>% filter(shop_id == 'S4')
set.seed(793)
sampled_customers4 <- s4 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s4 <- s4 %>%
  filter(customer_id %in% sampled_customers4$customer_id)

customer_products4 <- aggregate(product_id ~ customer_id, sampled_s4, function(x) unique(x))

clust_s4 <- customer_products4 %>%
  separate_rows(product_id, sep = ",") %>%
  group_by(customer_id) %>%
  mutate(cluster = rep(1:ceiling(n()/100), each = 100, length.out = n())) %>%
  ungroup() %>%
  select(customer_id, cluster) %>%
  distinct() %>%
  mutate(customer_id = paste0(customer_id, "_", cluster)) %>%
  arrange(customer_id)  # Optional: arrange by customer_id for better readability

# View the new dataframe
print(clust_s4)

unique_customers <- unique(clust_s4 $customer_id)
unique_categories <- unique(clust_s4 $cluster)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(clust_s4 )) {
  customer1 <- clust_s4 $customer_id[i]
  category <- clust_s4 $cluster[i]
  
  # Find other customers with the same category
  other_customers <- clust_s4 $customer_id[clust_s4 $cluster == category & 
                                             clust_s4 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set diversity as node sizes
V(g)$colour <- as.factor (clust_s4$cluster)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(alpha = 0.5, size = 0.5) +
  theme_void()+
  guides(color = none) 

# S5 ----
s5 <- whole_df %>% filter(shop_id == 'S5')
set.seed(793)
sampled_customers5 <- s5 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s5 <- s5 %>%
  filter(customer_id %in% sampled_customers5$customer_id)

customer_products5 <- aggregate(product_id ~ customer_id, sampled_s5, function(x) unique(x))

clust_s5 <- customer_products5 %>%
  separate_rows(product_id, sep = ",") %>%
  group_by(customer_id) %>%
  mutate(cluster = rep(1:ceiling(n()/100), each = 100, length.out = n())) %>%
  ungroup() %>%
  select(customer_id, cluster) %>%
  distinct() %>%
  mutate(customer_id = paste0(customer_id, "_", cluster)) %>%
  arrange(customer_id)  # Optional: arrange by customer_id for better readability

# View the new dataframe
print(clust_s5)

unique_customers <- unique(clust_s5 $customer_id)
unique_categories <- unique(clust_s5 $cluster)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(clust_s5 )) {
  customer1 <- clust_s5 $customer_id[i]
  category <- clust_s5 $cluster[i]
  
  # Find other customers with the same category
  other_customers <- clust_s5 $customer_id[clust_s5 $cluster == category & 
                                             clust_s5 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set diversity as node sizes
V(g)$colour <- as.factor (clust_s5$cluster)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(alpha = 0.5, size = 0.5) +
  theme_void()+
  guides(color = none) 

#----

find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers1 <- nrow(customer_products1)
adj_matrix1 <- matrix(0, nrow = num_customers1, ncol = num_customers1)

# Compare product lists between customers
for (i in 1:num_customers1) {
  for (j in (i+1):num_customers1) {
    common_count <- find_common_products(customer_products1$product_id[i], customer_products1$product_id[j])
    if (common_count >= 10) {
      adj_matrix1[i, j] <- 1
      adj_matrix1[j, i] <- 1
    }
  }
}

g1 <- graph.adjacency(adj_matrix1, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g1, layout = layout_with_fr, vertex.label = customer_products1$customer_id)

# # Plot the graph using ggraph
# ggraph(g, layout = 'fr') +
#   geom_edge_link(aes(width = weight), edge_colour = "gray50") +
#   geom_node_point(color = "skyblue", size = 1) +
#   geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
#   theme_void()

# S2 ----

s2 <- whole_df %>% filter(shop_id == 'S2')
set.seed(793)
sampled_customers2 <- s2 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s2 <- s2 %>%
  filter(customer_id %in% sampled_customers2$customer_id)

customer_products2 <- aggregate(product_id ~ customer_id, sampled_s2, function(x) unique(x))
find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers2 <- nrow(customer_products2)
adj_matrix2 <- matrix(0, nrow = num_customers2, ncol = num_customers2)

# Compare product lists between customers
for (i in 1:num_customers2) {
  for (j in (i+1):num_customers2) {
    common_count <- find_common_products(customer_products2$product_id[i], customer_products2$product_id[j])
    if (common_count >= 10) {
      adj_matrix2[i, j] <- 1
      adj_matrix2[j, i] <- 1
    }
  }
}

g2 <- graph.adjacency(adj_matrix2, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g2, layout = layout_with_fr, vertex.label = customer_products2$customer_id)

# # Plot the graph using ggraph
# ggraph(g, layout = 'fr') +
#   geom_edge_link(aes(width = weight), edge_colour = "gray50") +
#   geom_node_point(color = "skyblue", size = 1) +
#   geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
#   theme_void()

# S3 ----

s3 <- whole_df %>% filter(shop_id == 'S3')
set.seed(793)
sampled_customers3 <- s3 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s3 <- s3 %>%
  filter(customer_id %in% sampled_customers3$customer_id)

customer_products3 <- aggregate(product_id ~ customer_id, sampled_s3, function(x) unique(x))
find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers3 <- nrow(customer_products3)
adj_matrix3 <- matrix(0, nrow = num_customers3, ncol = num_customers3)

# Compare product lists between customers
for (i in 1:num_customers3) {
  for (j in (i+1):num_customers3) {
    common_count <- find_common_products(customer_products3$product_id[i], customer_products3$product_id[j])
    if (common_count >= 10) {
      adj_matrix3[i, j] <- 1
      adj_matrix3[j, i] <- 1
    }
  }
}

g3 <- graph.adjacency(adj_matrix3, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g3, layout = layout_with_fr, vertex.label = customer_products3$customer_id)

# # Plot the graph using ggraph
# ggraph(g, layout = 'fr') +
#   geom_edge_link(aes(width = weight), edge_colour = "gray50") +
#   geom_node_point(color = "skyblue", size = 1) +
#   geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
#   theme_void()

# S4 ----

s4 <- whole_df %>% filter(shop_id == 'S4')
set.seed(793)
sampled_customers4 <- s4 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s4 <- s4 %>%
  filter(customer_id %in% sampled_customers4$customer_id)

customer_products4 <- aggregate(product_id ~ customer_id, sampled_s4, function(x) unique(x))
find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers4 <- nrow(customer_products4)
adj_matrix4 <- matrix(0, nrow = num_customers4, ncol = num_customers4)

# Compare product lists between customers
for (i in 1:num_customers4) {
  for (j in (i+1):num_customers4) {
    common_count <- find_common_products(customer_products4$product_id[i], customer_products4$product_id[j])
    if (common_count >= 10) {
      adj_matrix4[i, j] <- 1
      adj_matrix4[j, i] <- 1
    }
  }
}

g4 <- graph.adjacency(adj_matrix4, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g4, layout = layout_with_fr, vertex.label = customer_products4$customer_id)

# # Plot the graph using ggraph
# ggraph(g, layout = 'fr') +
#   geom_edge_link(aes(width = weight), edge_colour = "gray50") +
#   geom_node_point(color = "skyblue", size = 1) +
#   geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
#   theme_void()

# S5 ----

s5 <- whole_df %>% filter(shop_id == 'S5')
set.seed(793)
sampled_customers5 <- s5 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s5 <- s5 %>%
  filter(customer_id %in% sampled_customers5$customer_id)

customer_products5 <- aggregate(product_id ~ customer_id, sampled_s5, function(x) unique(x))
find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers5 <- nrow(customer_products5)
adj_matrix5 <- matrix(0, nrow = num_customers5, ncol = num_customers5)

# Compare product lists between customers
for (i in 1:num_customers5) {
  for (j in (i+1):num_customers5) {
    common_count <- find_common_products(customer_products5$product_id[i], customer_products5$product_id[j])
    if (common_count >= 10) {
      adj_matrix5[i, j] <- 1
      adj_matrix5[j, i] <- 1
    }
  }
}

g5 <- graph.adjacency(adj_matrix5, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g5, layout = layout_with_fr, vertex.label = customer_products5$customer_id)

# # Plot the graph using ggraph
# ggraph(g, layout = 'fr') +
#   geom_edge_link(aes(width = weight), edge_colour = "gray50") +
#   geom_node_point(color = "skyblue", size = 1) +
#   geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
#   theme_void()
