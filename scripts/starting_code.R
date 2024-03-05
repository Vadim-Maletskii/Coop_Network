library(dplyr)
library(igraph)
library(ggraph)

purchases <- read.csv('data_orig/supermarket_purchases.csv', sep = ' ')
distances <- read.csv('data_orig/supermarket_distances.csv', sep = ' ')
prices <- read.csv('data_orig/supermarket_prices.csv', sep = ' ')

cusprod <- purchases %>% filter(shop_id == 1) %>%  select(customer_id, product_id)
cusprod$customer_id <- paste("C", cusprod$customer_id, sep = "")
cusprod$product_id <- paste("V", cusprod$product_id, sep = "")
cusprod_matrix <- as.matrix(cusprod)
cusprod_net <- graph_from_edgelist(cusprod_matrix)

# cusprod_net <- graph_from_data_frame(cusprod, vertices = cusprod$customer_id)

edge_density(cusprod_net)
sort(degree(cusprod_net, mode='in'), decreasing=T)
degree_distribution(cusprod_net)

# cusprod_net %>% 
#   degree(mode = "in") %>% 
#   hist(,
#        col = "red",
#        xlab = "In-degree",
#        ylab = "Frequency",
#        main = "Support",
#        breaks = min(.):max(.)
#   )


#Node attributes ----
