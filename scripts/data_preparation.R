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
# rio::export(purchases, 'whole_df.csv', format = 'csv')
