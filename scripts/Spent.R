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
