#library(config)
library(dplyr)
library(lubridate)
library(plotly)
library(arules)
library(igraph)
library(arulesViz)


# Read Configruations

#config.contents <- config::get(file = "conf/config.yml")


# Read Input Data
txn.data <- read.csv('data/Transaction_1.csv')

cities <- unique(txn.data$CITY)

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





# Association Rule Mining

get.txn <- function(data.path, columns){
  # Get transaction object for a given data file
  #
  # Args:
  #  data.path:  data file name location
  #  columns: transaction id and item id columns.
  #
  # Returns:
  #   transaction object
  transactions.obj <- read.transactions(file = data.path, format = "single", 
                                        sep = ",",
                                        cols = columns, 
                                        rm.duplicates = FALSE,
                                        quote = "", skip = 0,
                                        encoding = "unknown")
  return(transactions.obj)
}


get.rules <- function(support, confidence, transactions){
  # Get Apriori rules for given support and confidence values
  #
  # Args:
  #  support: support parameter
  #  confidence: confidence parameter
  #
  # Returns:
  #  rules object
  parameters = list(
    support = support,
    confidence = confidence,
    minlen  = 2,  # Minimal number of items per item set
    maxlen  = 10, # Maximal number of items per item set
    target  = "rules"
    
  )
  
  rules <- apriori(transactions, parameter = parameters)
  return(rules)
}

find.rules <- function(transactions, support, confidence, topN = 10){
  # Generate and prune the rules for given support confidence value
  #
  # Args:
  #  transactions: Transaction object, list of transactions
  #  support: Minimum support threshold
  #  confidence: Minimum confidence threshold
  # Returns:
  #  A data frame with the best set of rules and their support and confidence values
  
  
  # Get rules for given combination of support and confidence
  all.rules <- get.rules(support, confidence, transactions)
  
  rules.df <-data.frame(rules = labels(all.rules)
                        , all.rules@quality)
  
  other.im <- interestMeasure(all.rules, transactions = transactions)
  
  rules.df <- cbind(rules.df, other.im[,c('conviction','leverage')])
  
  
  # Keep the best rule based on the interest measure
  best.rules.df <- head(rules.df[order(-rules.df$leverage),],topN)
  
  return(best.rules.df)
}

plot.graph <- function(cross.sell.rules){
  # Plot the associated items as graph
  #
  # Args:
  #  cross.sell.rules: Set of final rules recommended
  # Returns:
  #  None
  edges <- unlist(lapply(cross.sell.rules['rules'], strsplit, split='=>'))
  g <- graph(edges = edges)
  return(g)
  
}

columns <- c("ORDER_ID", "PDT_CAT_1") ## columns of interest in data file
data.path = 'data/Transaction_1.csv'  ## Path to data file
transactions.obj <- get.txn(data.path, columns) ## create txn object

