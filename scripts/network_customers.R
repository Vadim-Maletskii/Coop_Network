s4 <- whole_df %>% filter(shop_id == 'S4')
set.seed(793)
sampled_customers <- s4 %>%
  distinct(customer_id) %>% sample_n(500)
sampled_s4 <- s4 %>%
  filter(customer_id %in% sampled_customers$customer_id)

customer_products <- aggregate(product_id ~ customer_id, sampled_s4, function(x) unique(x))
find_common_products <- function(products1, products2) {
  common <- intersect(unlist(products1), unlist(products2))
  return(length(common))
}

# Create an empty adjacency matrix
num_customers <- nrow(customer_products)
adj_matrix <- matrix(0, nrow = num_customers, ncol = num_customers)

# Compare product lists between customers
for (i in 1:num_customers) {
  for (j in (i+1):num_customers) {
    common_count <- find_common_products(customer_products$product_id[i], customer_products$product_id[j])
    if (common_count >= 100) {
      adj_matrix[i, j] <- 1
      adj_matrix[j, i] <- 1
    }
  }
}

g <- graph.adjacency(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Plot the graph
plot(g, layout = layout_with_fr, vertex.label = customer_products$customer_id)

# Plot the graph using ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), edge_colour = "gray50") +
  geom_node_point(color = "skyblue", size = 1) +
  geom_node_text(aes(label = customer_id), repel = TRUE) + # Corrected to 'customer_id'
  theme_void()
