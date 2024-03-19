# S1 ----

s1 <- whole_df %>% filter(shop_id == 'S1')
set.seed(793)
sampled_customers1 <- s1 %>%
  distinct(customer_id) %>% sample_n(100)
sampled_s1 <- s1 %>%
  filter(customer_id %in% sampled_customers1$customer_id)

customer_products1 <- aggregate(product_id ~ customer_id, sampled_s1, function(x) unique(x))
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
