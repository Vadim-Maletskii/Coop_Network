avg_products_overall <- whole_df %>%
  group_by(customer_id) %>%
  summarize(avg_products = mean(quantity))
mean(avg_products_overall$avg_products)

# Calculate the average number of products bought by customers by shop_id
avg_products_by_shop <- whole_df %>%
  group_by(shop_id, customer_id) %>%
  summarize(avg_products = mean(quantity)) %>%
  group_by(shop_id) %>%
  summarize(avg_products = mean(avg_products))

# Combine the two data frames
combined_df <- rbind(
  data.frame(shop_id = "Overall", avg_products = mean(avg_products_overall$avg_products)),
  avg_products_by_shop
)

# Plotting
ggplot(combined_df, aes(x = shop_id, y = avg_products, fill = shop_id)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Number of Products Bought by Customers",
       x = "Shop ID",
       y = "Average Number of Products") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

total_earnings_by_shop <- whole_df %>%
  group_by(shop_id) %>%
  summarize(total_earnings = sum(price * quantity))

# Plotting
ggplot(total_earnings_by_shop, aes(x = shop_id, y = total_earnings, fill = shop_id)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Earnings by Shop",
       x = "Shop ID",
       y = "Total Earnings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
