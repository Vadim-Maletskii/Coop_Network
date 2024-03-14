library(dplyr)
library(igraph)
library(ggraph)

whole_df <- read.csv('data_preproc/whole_df.csv')
sampled_df <- read.csv('data_preproc/sampled_df.csv')

# Checking how many customers made purchases in all five shops (to mention in the paper) ----
customer_shop_count <- whole_df %>%
  group_by(customer_id) %>%
  summarise(num_shops = n_distinct(shop_id))

customers_with_five_shops <- customer_shop_count %>%
  filter(num_shops >= 5) # 1742

table(customer_shop_count$num_shops) # most people (24362) made purchases in two shops

sampled_customers <- customers_with_five_shops %>%
  sample_n(5000)

# Whole df ----
whole_df <- whole_df %>%
  group_by(customer_id) %>%
  mutate(shop_category = factor(dense_rank(distance)))

# H1 whole df:
whole_df %>% group_by(shop_category) %>% summarize(sum_products = sum(quantity)) %>% mutate(percent = sum_products / sum(sum_products) * 100)
sum_quantity_grouped_whole <- whole_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
quantity_percent_in_shops <- sum_quantity_grouped_whole %>% group_by(shop_category) %>% summarize(avg_percent = round(mean(percentage), 2))

pie_chart_percent_in_shops <- ggplot(quantity_percent_in_shops, aes(x = "", y = avg_percent, fill = shop_category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(avg_percent, "%")), position = position_stack(vjust = 0.5)) +  # Add text labels for avg_percent
  coord_polar("y", start = 0) +  # Convert bar plot to pie chart
  labs(title = "Distribution of Items (%) Bought by Customer in Each Shop") +
  theme_void() +  # Remove unnecessary elements
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +  # Adjust legend position if needed
  scale_fill_brewer()

# H2 whole df:
sum_spent_grouped <- whole_df %>% group_by(customer_id, shop_category) %>%
  summarize(sum_spent = sum(quantity * price)) %>% mutate(percent = sum_spent / sum(sum_spent) * 100)
sum_percent_in_shops <- sum_spent_grouped %>% group_by(shop_category) %>% summarize(avg_percent = round(mean(percent), 2))

pie_chart_spent_in_shops <- ggplot(sum_percent_in_shops, aes(x = "", y = avg_percent, fill = shop_category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(avg_percent, "%")), position = position_stack(vjust = 0.5)) +  # Add text labels for avg_percent
  coord_polar("y", start = 0) +  # Convert bar plot to pie chart
  labs(title = "Distribution of Money Spent (%) by Customer in Each Shop") +
  theme_void() +  # Remove unnecessary elements
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +  # Adjust legend position if needed
  scale_fill_brewer(palette = 'Greens')

# Sampled df ----
sampled_df <- sampled_df %>%
  group_by(customer_id) %>%
  mutate(shop_category = dense_rank(distance))

# H1 sampled df:
sampled_df %>% group_by(shop_category) %>% summarize(sum_products = sum(quantity)) %>% mutate(percent = sum_products / sum(sum_products) * 100)
sum_p_grouped <- sampled_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
sum_p_grouped %>% group_by(shop_category) %>% summarize(avg_percent = mean(percentage))

distances <- distances %>%
  group_by(customer_id) %>%
  mutate(shop_category = factor(dense_rank(distance)))

# plot(distance_q$distance, distance_q$sum_products, ylim = c(0, 10000),
#      xlab = "Distance", ylab = "Sum of Products", 
#      main = "Relationship between Distance and Sum of Products")



# H2 sampled df:
sampled_df %>% group_by(customer_id, shop_category) %>% summarize(sum_spent = sum(quantity * price)) %>% mutate(percent = sum_spent / sum(sum_spent) * 100)
sum_spent_grouped <- sampled_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
sum_p_grouped %>% group_by(shop_category) %>% summarize(avg_percent = mean(percentage))
     