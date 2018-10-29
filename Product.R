library(ggplot2)
library(scales)
library(dplyr)
library(cowplot)
library(arules)
library(arules)
library(igraph)
setwd("C:/Geetha/Ondipuli/RMKV")




file <-  read.csv("Transaction.csv")

file_1 <- subset(file, select = c(ORDER_ID, PDT_CAT_1))
write.csv(file_1, file = "txn.csv")

transactions.obj <- read.transactions(file = "txn.csv", format = "single", 
                                      sep = ",",
                                      cols = c("ORDER_ID", "PDT_CAT_1"), 
                                      rm.duplicates = FALSE,
                                      quote = '"', skip = 0,
                                      encoding = "unknown")
transactions.obj

as.data.frame(head(sort(itemFrequency(transactions.obj, type = "absolute")
                        , decreasing = TRUE), 10) )  # Most  frequent

as.data.frame(head(sort(itemFrequency(transactions.obj, type = "absolute")
                        , decreasing = FALSE), 10))  # Least frequent

itemFrequencyPlot(transactions.obj,topN = 20)


########################################################################################


# Interest Measures
support    <- 0.01

# Frequent item sets
parameters = list(
  support = support,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 10, # Maximal number of items per item set
  target  = "frequent itemsets"
)

freq.items <- apriori(transactions.obj, parameter = parameters)
freq.items.df <- data.frame(item_set = labels(freq.items)
                            , support = freq.items@quality)

head(freq.items.df, 5)
tail(freq.items.df, 5)

head(freq.items.df[order(freq.items.df$support.support, decreasing = TRUE),],10)
###########################################################################################

confidence <- 0.2 # Interest Measure
parameters = list(
  support = support,
  confidence = confidence,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 10, # Maximal number of items per item set
  target  = "rules"
)

rules <- apriori(transactions.obj, parameter = parameters)
rules.df <- data.frame(rules = labels(rules)
                       ,rules@quality)
head(rules.df)
tail(rules.df)

#############################################################################################

get.txn <- function(data.path, columns){
  transactions.obj <- read.transactions(file = data.path, format = "single", 
                                        sep = ",",
                                        cols = columns, 
                                        rm.duplicates = FALSE,
                                        quote = '"', skip = 0,
                                        encoding = "unknown")
  return(transactions.obj)
}


get.rules <- function(support, confidence, transactions){
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

plot.graph <- function(cross.sell.rules){
  # Plot the associated items as graph
  edges <- unlist(lapply(cross.sell.rules['rules'], strsplit, split='=>'))
  
  g <- graph(edges = edges)
  plot(g)
  
}
find.rules <- function(transactions, support, confidence, topN = 10){
  all.rules <- get.rules(support, confidence, transactions)
  
  rules.df <-data.frame(rules = labels(all.rules)
                        , all.rules@quality)
  
  other.im <- interestMeasure(all.rules, transactions = transactions)
  
  rules.df <- cbind(rules.df, other.im[,c('conviction','leverage')])
  
  
  # Keep the best rule based on the interest measure
  best.rules.df <- head(rules.df[order(-rules.df$leverage),],topN)
  
  return(best.rules.df)
}

support <- 0.01
confidence <- 0.2

columns <- c("ORDER_ID", "PDT_CAT_1") ## columns of interest in data file
data.path = 'txn.csv'  ## Path to data file

transactions.obj <- get.txn(data.path, columns) ## create txn object

cross.sell.rules <- find.rules( transactions.obj, support, confidence )
cross.sell.rules$rules <- as.character(cross.sell.rules$rules)              

plot.graph(cross.sell.rules)
##############################################################################



get.neg.rules <- function(transactions, itemList, support, confidence){
  
  neg.transactions <- addComplement(transactions.obj, labels = itemList)
  rules <- find.rules(neg.transactions, support, confidence)
  return(rules)
}

itemList <- c("Dhotis")
neg.rules <- get.neg.rules(transactions.obj, itemList , .05,.6)
neg.rules.nr <- neg.rules[!is.redundant((neg.rules))]
labels(neg.rules)

















#############################################################################
tbl <- crossTable(transactions.obj, sort = T)
tbl
















