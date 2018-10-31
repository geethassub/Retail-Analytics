library(config)
library(dplyr)

# Read Configruations

config.contents <- config::get(file = "conf/config.yml")


# Read Input Data
txn.data <- read.csv('data/Transaction.csv')

# YTD Sales
ytd.sales <- txn.data %>% summarise(TotalSales = sum(Total_Price)) 
ytd.sales <- ytd.sales$TotalSales

# YRS Orders
ytd.orders <- txn.data %>% summarise(TotalOrders = n_distinct(ORDER_ID))
ytd.orders <- ytd.orders$TotalOrders

# Avg Monthly
