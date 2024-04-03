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
  mutate(spent = sum(price * quantity))


dfs_list <- split(sampled_df, sampled_df$shop_id)

# Access individual dataframes from the list
shop_id_S1 <- dfs_list$S1
shop_id_S2 <- dfs_list$S2
shop_id_S3 <- dfs_list$S3
shop_id_S4 <- dfs_list$S4
shop_id_S5 <- dfs_list$S5

shop_id_S1 <- shop_id_S1 %>%
  select(spent, customer_id, shop_category)
shop_id_S2 <- shop_id_S2 %>%
  select(spent, customer_id, shop_category)
shop_id_S3 <- shop_id_S3 %>%
  select(spent, customer_id, shop_category)
shop_id_S4 <- shop_id_S4 %>%
  select(spent, customer_id, shop_category)
shop_id_S5 <- shop_id_S5 %>%
  select(spent, customer_id, shop_category)

shop_id_S1 <- distinct (shop_id_S1)
shop_id_S2 <- distinct (shop_id_S2)
shop_id_S3 <- distinct (shop_id_S3)
shop_id_S4 <- distinct (shop_id_S4)
shop_id_S5 <- distinct (shop_id_S5)


set.seed(793)
shop_id_S1 <- shop_id_S1[sample(nrow(shop_id_S1), 100), ]
shop_id_S2 <- shop_id_S2[sample(nrow(shop_id_S2), 100), ]
shop_id_S3 <- shop_id_S3[sample(nrow(shop_id_S3), 100), ]
shop_id_S4 <- shop_id_S4[sample(nrow(shop_id_S4), 100), ]
shop_id_S5 <- shop_id_S5[sample(nrow(shop_id_S5), 100), ]

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

# Set spent as node sizes
V(g)$size <- shop_id_S1 $spent
V(g)$colour <- as.factor (shop_id_S1$shop_category)

# Plot with manual colors
gplot = ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 5)) + 
  scale_colour_manual(values = category_colors) +
  theme_graph()+
  theme(legend.text = element_text(size = 10),  # Increase legend text size
        legend.title = element_text(size = 12))+ 
  labs(color = "Shop category", size = "Overall spent")
ggsave("plots/plot21.png", plot = gplot, width = 19.2, height = 9.92, units = "cm", dpi = 300)
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

# Set spent as node sizes
V(g)$size <- shop_id_S2 $spent
V(g)$colour <- as.factor (shop_id_S2$shop_category)


# Plot with manual colors
gplot = ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 5)) + 
  scale_colour_manual(values = category_colors) +
  theme_graph()+
  theme(legend.text = element_text(size = 10),  # Increase legend text size
        legend.title = element_text(size = 12))+ 
  labs(color = "Shop category", size = "Overall spent")
ggsave("plots/plot22.png", plot = gplot, width = 19.2, height = 9.92, units = "cm", dpi = 300)

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

# Set spent as node sizes
V(g)$size <- shop_id_S3 $spent
V(g)$colour <- as.factor (shop_id_S3$shop_category)

# Plot with manual colors
gplot = ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 5)) + 
  scale_colour_manual(values = category_colors) +
  theme_graph()+
  theme(legend.text = element_text(size = 10),  # Increase legend text size
        legend.title = element_text(size = 12))+ 
  labs(color = "Shop category", size = "Overall spent")
ggsave("plots/plot23.png", plot = gplot, width = 19.2, height = 9.92, units = "cm", dpi = 300)


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

# Set spent as node sizes
V(g)$size <- shop_id_S4$spent
V(g)$colour <- as.factor (shop_id_S4$shop_category)


# Plot with manual colors
gplot = ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 5)) + 
  scale_colour_manual(values = category_colors) +
  theme_graph()+
  theme(legend.text = element_text(size = 10),  # Increase legend text size
        legend.title = element_text(size = 12))+ 
  labs(color = "Shop category", size = "Overall spent")
ggsave("plots/plot24.png", plot = gplot, width = 19.2, height = 9.92, units = "cm", dpi = 300)

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

# Set spent as node sizes
V(g)$size <- shop_id_S5 $spent
V(g)$colour <- as.factor (shop_id_S5$shop_category)


# Plot with manual colors
gplot = ggraph(g, layout = "fr") +
  geom_edge_link(color = "white") +
  geom_node_point(aes(size = size, colour = colour), alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 5)) + 
  scale_colour_manual(values = category_colors) +
  theme_graph()+
  theme(legend.text = element_text(size = 10),  # Increase legend text size
        legend.title = element_text(size = 12))+ 
  labs(color = "Shop category", size = "Overall spent")
ggsave("plots/plot25.png", plot = gplot, width = 19.2, height = 9.92, units = "cm", dpi = 300)



