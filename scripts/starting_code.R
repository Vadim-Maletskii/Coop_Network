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

# H1: the closer the shop is located to the customer, the more products they tend to buy there
# create categories of shops: the closest, second closest, ... the farthest
# create boxplot: x-axis: categories of shops, y-axis: average number of products bought by customers
# 
# H2: the same with prices (the closer the shop, the more the customer spends there)
# 
# products bought by most customers (also the same in each particular shop)
# products bought most by quantity
# 
# average number of products bought by customers
# 
# Which shops made the most in total
#
# H4: Average amount of money spent may vary significantly from shop to shop (may be because 
# of geoposition of shops, but we don't have data to check it)
#
# ?? should we use all 24 mln observations?? or just take a sample?
# ?? the structure
