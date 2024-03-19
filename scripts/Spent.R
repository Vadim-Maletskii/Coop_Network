library(tidyverse)
library(dplyr)
library(igraph)
library(ggraph)

sampled_df <- read.csv('data_preproc/sampled_df.csv')

sampled_df <- sampled_df %>%
  group_by(customer_id) %>%
  mutate(shop_category = dense_rank(distance))

sampled_df <- sampled_df %>%
  group_by(customer_id) %>%
  mutate(avg_spent = mean(price * quantity))


dfs_list <- split(sampled_df, sampled_df$shop_id)

# Access individual dataframes from the list
shop_id_S1 <- dfs_list$S1
shop_id_S2 <- dfs_list$S2
shop_id_S3 <- dfs_list$S3
shop_id_S4 <- dfs_list$S4
shop_id_S5 <- dfs_list$S5

shop_id_S1 <- shop_id_S1 %>%
  select(avg_spent, customer_id, shop_category)
shop_id_S2 <- shop_id_S2 %>%
  select(avg_spent, customer_id, shop_category)
shop_id_S3 <- shop_id_S3 %>%
  select(avg_spent, customer_id, shop_category)
shop_id_S4 <- shop_id_S4 %>%
  select(avg_spent, customer_id, shop_category)
shop_id_S5 <- shop_id_S5 %>%
  select(avg_spent, customer_id, shop_category)

shop_id_S1 <- distinct (shop_id_S1)
shop_id_S2 <- distinct (shop_id_S2)
shop_id_S3 <- distinct (shop_id_S3)
shop_id_S4 <- distinct (shop_id_S4)
shop_id_S5 <- distinct (shop_id_S5)


set.seed(793)
shop_id_S1 <- shop_id_S1[sample(nrow(shop_id_S1), 250), ]
shop_id_S2 <- shop_id_S2[sample(nrow(shop_id_S2), 250), ]
shop_id_S3 <- shop_id_S3[sample(nrow(shop_id_S3), 250), ]
shop_id_S4 <- shop_id_S4[sample(nrow(shop_id_S4), 250), ]
shop_id_S5 <- shop_id_S5[sample(nrow(shop_id_S5), 250), ]

category_colors = c("red", "blue", "green","orange","purple")

#----
# Create a list of unique customer IDs and shop categories
unique_customers <- unique(shop_id_S1 $customer_id)
unique_categories <- unique(shop_id_S1 $shop_category)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(shop_id_S1 )) {
  customer1 <- shop_id_S1 $customer_id[i]
  category <- shop_id_S1 $shop_category[i]
  
  # Find other customers with the same category
  other_customers <- shop_id_S1 $customer_id[shop_id_S1 $shop_category == category & 
                                               shop_id_S1 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set avg_spent as node sizes
V(g)$size <- shop_id_S1 $avg_spent
V(g)$colour <- as.factor (shop_id_S1$shop_category)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) + 
  scale_colour_manual(values = category_colors) +
  theme_void()

#----
# Create a list of unique customer IDs and shop categories
unique_customers <- unique(shop_id_S2 $customer_id)
unique_categories <- unique(shop_id_S2 $shop_category)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(shop_id_S2 )) {
  customer1 <- shop_id_S2 $customer_id[i]
  category <- shop_id_S2 $shop_category[i]
  
  # Find other customers with the same category
  other_customers <- shop_id_S2 $customer_id[shop_id_S2 $shop_category == category & 
                                               shop_id_S2 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set avg_spent as node sizes
V(g)$size <- shop_id_S2 $avg_spent
V(g)$colour <- as.factor (shop_id_S2$shop_category)


# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) + 
  scale_colour_manual(values = category_colors) +
  theme_void()


#----

# Create a list of unique customer IDs and shop categories
unique_customers <- unique(shop_id_S3 $customer_id)
unique_categories <- unique(shop_id_S3 $shop_category)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(shop_id_S3 )) {
  customer1 <- shop_id_S3 $customer_id[i]
  category <- shop_id_S3 $shop_category[i]
  
  # Find other customers with the same category
  other_customers <- shop_id_S3 $customer_id[shop_id_S3 $shop_category == category & 
                                               shop_id_S3 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set avg_spent as node sizes
V(g)$size <- shop_id_S3 $avg_spent
V(g)$colour <- as.factor (shop_id_S3$shop_category)

# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) + 
  scale_colour_manual(values = category_colors) +
  theme_void()


#----

# Create a list of unique customer IDs and shop categories
unique_customers <- unique(shop_id_S4 $customer_id)
unique_categories <- unique(shop_id_S4 $shop_category)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(shop_id_S4 )) {
  customer1 <- shop_id_S4 $customer_id[i]
  category <- shop_id_S4 $shop_category[i]
  
  # Find other customers with the same category
  other_customers <- shop_id_S4 $customer_id[shop_id_S4 $shop_category == category & 
                                               shop_id_S4 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set avg_spent as node sizes
V(g)$size <- shop_id_S4$avg_spent
V(g)$colour <- as.factor (shop_id_S4$shop_category)


# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) + 
  scale_colour_manual(values = category_colors) +
  theme_void()

#----
# Create a list of unique customer IDs and shop categories
unique_customers <- unique(shop_id_S5 $customer_id)
unique_categories <- unique(shop_id_S5 $shop_category)

# Create an empty adjacency matrix with dimension names
adj_matrix <- matrix(0, nrow = length(unique_customers), 
                     ncol = length(unique_customers),
                     dimnames = list(unique_customers, unique_customers))

# Loop through the dataframe to update adjacency matrix
for (i in 1:nrow(shop_id_S5 )) {
  customer1 <- shop_id_S5 $customer_id[i]
  category <- shop_id_S5 $shop_category[i]
  
  # Find other customers with the same category
  other_customers <- shop_id_S5 $customer_id[shop_id_S5 $shop_category == category & 
                                               shop_id_S5 $customer_id != customer1]
  
  # Update adjacency matrix for each pair of customers with the same category
  adj_matrix[customer1, other_customers] <- 1
}

# Convert adjacency matrix to igraph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

# Set avg_spent as node sizes
V(g)$size <- shop_id_S5 $avg_spent
V(g)$colour <- as.factor (shop_id_S5$shop_category)


# Plot with manual colors
ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) + 
  scale_colour_manual(values = category_colors) +
  theme_void()

