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

whole_df <- merge(purchases, distances, by = c("customer_id", "shop_id"), all.x = TRUE)
rio::export(whole_df, 'data_preproc/whole_df.csv', format = 'csv')

set.seed(793)
sampled_customers <- whole_df %>%
  distinct(customer_id) %>%
  sample_n(5000)

sampled_df <- whole_df %>%
  filter(customer_id %in% sampled_customers$customer_id)

dim(sampled_df)
length(unique(sampled_df$customer_id))

rio::export(sampled_df, 'data_preproc/sampled_df.csv', format = 'csv')
 