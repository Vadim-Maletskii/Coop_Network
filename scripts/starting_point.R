sampled_df <- read.csv('data_preproc/sampled_df.csv', sep = ',')

cusprod <- sampled_df %>% select(customer_id, product_id)
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
# H3:  Нетворк по магазам с трешхолдом по опредленному кол-ву одинаково купленных товаров
# (с кластерами по товарам), даже если кластеров нет, можно посмотреть на разницу между магазами
#
# ??H4: нетворк для каждого магаза с дистанцией как длинна еджа??
# Which shops made the most in total
#
# projection: clusters of customers buying the same products in the same shop
# santo fortunato: community detection in graphs
# igraph: bipartite_...
#
# ?? should we use all 24 mln observations?? or just take a sample? TAKE A SAMPLE OF CUSTOMERS, ALSO LOOK AT THE NUMBER OF PRODUCTS
# the usual structure of the paper
# send the code