library(dplyr)
library(igraph)
library(ggraph)

purchases <- read.csv('data_orig/supermarket_purchases.csv', sep = ' ')
distances <- read.csv('data_orig/supermarket_distances.csv', sep = ' ')
prices <- read.csv('data_orig/supermarket_prices.csv', sep = ' ')

purchases <- merge(purchases, prices, by = "product_id", all.x = TRUE)

purchases$customer_id <- paste("C", purchases$customer_id, sep = "")
purchases$product_id <- paste("P", purchases$product_id, sep = "")
purchases$shop_id <- paste("S", purchases$shop_id, sep = "")

distances$customer_id <- paste("C", distances$customer_id, sep = "")
distances$shop_id <- paste("S", distances$shop_id, sep = "")

purchases <- merge(purchases, distances, by = c("customer_id", "shop_id"), all.x = TRUE)

cusprod <- purchases %>% select(customer_id, product_id)
cusprod_matrix <- as.matrix(cusprod)
cusprod_net <- graph_from_edgelist(cusprod_matrix)

# cusprod_net %>%  
#   degree(mode = "in") %>% 
#   hist(,
#        col = "red",
#        xlab = "In-degree",
#        ylab = "Frequency",
#        main = "Support",
#        breaks = min(.):max(.)
#   )

# edge_density(cusprod_net)
# sort(degree(cusprod_net, mode='in'), decreasing=T)
# degree_distribution(cusprod_net)


#Node attributes ----
