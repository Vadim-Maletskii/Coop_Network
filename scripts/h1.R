library(dplyr)
library(igraph)
library(ggraph)

sampled_df <- read.csv('data_preproc/sampled_df.csv')

sampled_df <- sampled_df %>%
  group_by(customer_id) %>%
  mutate(shop_category = dense_rank(distance))

# H1:
sampled_df %>% group_by(shop_category) %>% summarize(sum_products = sum(quantity)) %>% mutate(percent = sum_products / sum(sum_products) * 100)
sum_p_grouped <- sampled_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
sum_p_grouped %>% group_by(shop_category) %>% summarize(avg_percent = mean(percentage))

plot(distance_q$distance, distance_q$sum_products, ylim = c(0, 10000),
     xlab = "Distance", ylab = "Sum of Products", 
     main = "Relationship between Distance and Sum of Products")

# H2:
sampled_df %>% group_by(customer_id, shop_category) %>% summarize(sum_spent = sum(quantity * price)) %>% mutate(percent = sum_spent / sum(sum_spent) * 100)
sum_spent_grouped <- sampled_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
sum_p_grouped %>% group_by(shop_category) %>% summarize(avg_percent = mean(percentage))

     