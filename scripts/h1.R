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

# Whole df H1, H2 ----
whole_df <- whole_df %>%
  group_by(customer_id) %>%
  mutate(shop_category = factor(dense_rank(distance)))

# H1 whole df:

whole_df %>% group_by(shop_category) %>% summarize(dist_products = n()) %>% mutate(percent = dist_products / sum(dist_products) * 100)
dist_products_grouped_whole <- whole_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(dist_products = n()) %>%
  mutate(percentage = dist_products / sum(dist_products) * 100)
dist_products_grouped_whole <- dist_products_grouped_whole %>% group_by(shop_category) %>% summarize(avg_percent = round(mean(percentage), 2))

pie_chart_dist_products <- ggplot(dist_products_grouped_whole, aes(x = "", y = avg_percent, fill = shop_category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(avg_percent, "%")), position = position_stack(vjust = 0.5)) +  # Add text labels for avg_percent
  coord_polar("y", start = 0) +  # Convert bar plot to pie chart
  labs(title = "Distribution of Distinct Products (%) Bought by Customer in Each Shop") +
  theme_void() +  # Remove unnecessary elements
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +  # Adjust legend position if needed
  scale_fill_brewer(palette = 'Reds')

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



# H2 sampled df NOT FINISHED 
sampled_df %>% group_by(customer_id, shop_category) %>% summarize(sum_spent = sum(quantity * price)) %>% mutate(percent = sum_spent / sum(sum_spent) * 100)
sum_spent_grouped <- sampled_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
sum_p_grouped %>% group_by(shop_category) %>% summarize(avg_percent = mean(percentage))
     

# Products bought by most customers ----

# Overall
products_bought_overall <- whole_df %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_overall$percentage <- round((products_bought_overall$distinct_customers / 60365 * 100), 2) # 60365 is the number of distinct customers
top_10_overall <- head(products_bought_overall, 10)

ggplot(top_10_overall, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought Overall",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))
        
# S1
products_bought_s1 <- whole_df %>% filter(shop_id == 'S1')
distinct_customers_s1 <- n_distinct(products_bought_s1$customer_id)
products_bought_s1 <- products_bought_s1 %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_s1$percentage <- round((products_bought_s1$distinct_customers / distinct_customers_s1 * 100), 2)

top_10_s1 <- head(products_bought_s1, 10)

ggplot(top_10_s1, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought in Shop 1",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))

# S2
products_bought_s2 <- whole_df %>% filter(shop_id == 'S2')
distinct_customers_s2 <- n_distinct(products_bought_s2$customer_id)
products_bought_s2 <- products_bought_s2 %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_s2$percentage <- round((products_bought_s2$distinct_customers / distinct_customers_s2 * 100), 2)

top_10_s2 <- head(products_bought_s2, 10)

ggplot(top_10_s2, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "limegreen") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought in Shop 2",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))

# S3
products_bought_s3 <- whole_df %>% filter(shop_id == 'S3')
distinct_customers_s3 <- n_distinct(products_bought_s3$customer_id)
products_bought_s3 <- products_bought_s3 %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_s3$percentage <- round((products_bought_s3$distinct_customers / distinct_customers_s3 * 100), 2)

top_10_s3 <- head(products_bought_s3, 10)

ggplot(top_10_s3, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought in Shop 3",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))

# S4
products_bought_s4 <- whole_df %>% filter(shop_id == 'S4')
distinct_customers_s4 <- n_distinct(products_bought_s4$customer_id)
products_bought_s4 <- products_bought_s4 %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_s4$percentage <- round((products_bought_s4$distinct_customers / distinct_customers_s4 * 100), 2)

top_10_s4 <- head(products_bought_s4, 10)

ggplot(top_10_s4, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought in Shop 4",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))

# S5
products_bought_s5 <- whole_df %>% filter(shop_id == 'S5')
distinct_customers_s5 <- n_distinct(products_bought_s5$customer_id)
products_bought_s5 <- products_bought_s5 %>% group_by(product_id) %>% summarize(distinct_customers = n_distinct(customer_id)) %>% arrange(desc(distinct_customers))
products_bought_s5$percentage <- round((products_bought_s5$distinct_customers / distinct_customers_s5 * 100), 2)

top_10_s5 <- head(products_bought_s5, 10)

ggplot(top_10_s5, aes(x = reorder(product_id, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "brown") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  labs(title = "Top 10 Products Bought in Shop 5",
       x = "Product ID",
       y = "Percentage") +
  theme(axis.text.x = element_text(hjust = 1))

distances <- distances %>%
  group_by(customer_id) %>%
  mutate(shop_category = factor(dense_rank(distance)))

distances_with_purchase <- distances %>%
  left_join(whole_df %>%
              distinct(customer_id, shop_id) %>%
              mutate(purchase = TRUE),
            by = c("customer_id", "shop_id")) %>%
  mutate(purchase = ifelse(is.na(purchase), FALSE, TRUE))
filtered_distances <- distances_with_purchase[distances_with_purchase$purchase == TRUE, ]
table(distances$shop_id, distances$shop_category) # how much shops are close to people in general
filtered_distances %>% group_by(shop_id) %>% summarize(mean_distance = median(distance))

customer_purchase_counts <- distances_with_purchase %>%
  group_by(customer_id) %>%
  summarise(num_true = sum(purchase))

# Filter customers with exactly 5 TRUE values
customers_with_5_true <- customer_purchase_counts %>%
  filter(num_true == 5)

# Filter distances_with_purchase to keep only rows for customers with 5 TRUE values
filtered_distances <- distances_with_purchase %>%
  filter(customer_id %in% customers_with_5_true$customer_id)

# the percentage of distinct customers (and distinct products bought) is the highest for the shop S1, next is S2, next is S3, next is S5, next is S4
# it is like this even though S1 is not closest to most people, it is in category 4. the closest one is S3, then S5, then S2, then S1, then S4
selected_customer_ids <- unique(filtered_distances$customer_id)

# Filter whole_df to keep only rows with customer_ids present in selected_customer_ids
filtered_whole_df <- whole_df %>%
  filter(customer_id %in% selected_customer_ids)

filtered_whole_df %>% group_by(shop_category) %>% summarize(dist_products = n()) %>% mutate(percent = dist_products / sum(dist_products) * 100)
mean_distances <- filtered_whole_df %>% group_by(shop_category) %>% summarize(mean_distance = mean(distance))
sum_quantity_grouped_whole <- filtered_whole_df %>%
  group_by(customer_id, shop_category) %>%
  summarize(sum_products = sum(quantity)) %>%
  mutate(percentage = (sum_products / sum(sum_products)) * 100)
quantity_percent_in_shops <- sum_quantity_grouped_whole %>% group_by(shop_category) %>% summarize(avg_percent = round(mean(percentage), 2))
quantity_percent_in_shops <- merge(quantity_percent_in_shops, mean_distances)
quantity_percent_in_shops$mean_distance <- round(quantity_percent_in_shops$mean_distance)
pie_chart_percent_in_shops <- ggplot(quantity_percent_in_shops, aes(x = "", y = avg_percent, fill = shop_category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(avg_percent, "%")), position = position_stack(vjust = 0.5)) +  # Add text labels for avg_percent
  coord_polar("y", start = 0) +  # Convert bar plot to pie chart
  labs(title = "Distribution of Items (%) Bought by Customer in Each Shop") +
  theme_void() +  # Remove unnecessary elements
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +  # Adjust legend position if needed
  scale_fill_brewer()

