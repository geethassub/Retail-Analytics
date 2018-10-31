library(config)
library(dplyr)
library(lubridate)


# Read Configruations

config.contents <- config::get(file = "conf/config.yml")


# Read Input Data
txn.data <- read.csv('data/Transaction_1.csv')


# Clean up
txn.data$Month <- factor(txn.data$Month, levels = c( "January", "February","March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ))
txn.data$Day_1 <- factor(txn.data$Day_1, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
txn.data$QUANTITY <- as.numeric(txn.data$QUANTITY)
txn.data$UNIT_PRICE <- as.numeric(txn.data$UNIT_PRICE)
txn.data$Total_Price <- as.numeric(txn.data$Total_Price)

txn.data$age_bin[txn.data$AGE < 20] = "Less than 20"
txn.data$age_bin[txn.data$AGE >= 20 & txn.data$AGE < 30] = "20 to 30"
txn.data$age_bin[txn.data$AGE >= 30 & txn.data$AGE < 40] = "30 to 40"
txn.data$age_bin[txn.data$AGE >= 40 & txn.data$AGE < 50] = "40 to 50"
txn.data$age_bin[txn.data$AGE >= 50 & txn.data$AGE < 60] = "50 to 60"
txn.data$age_bin[txn.data$AGE >= 60] = "60 and above"
txn.data$Date <- mdy(txn.data$Date)




# YTD Sales
ytd.sales <- txn.data %>% summarise(TotalSales = sum(Total_Price)) 
ytd.sales <- ytd.sales$TotalSales

# YRS Orders
ytd.orders <- txn.data %>% summarise(TotalOrders = n_distinct(ORDER_ID))
ytd.orders <- ytd.orders$TotalOrders

# Avg Monthly
