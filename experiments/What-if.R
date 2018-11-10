library(ggplot2)
library(scales)
library(dplyr)
library(cowplot)
library(lubridate)
library(arules)
library(arules)
library(igraph)


setwd("C:/Geetha/Ondipuli/RMKV")
file <- read.csv("Transaction.csv")

names(file)
file$Month <- factor(file$Month, levels = c( "January", "February","March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ))
file$Day_1 <- factor(file$Day_1, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
file$QUANTITY <- as.numeric(file$QUANTITY)
file$UNIT_PRICE <- as.numeric(file$UNIT_PRICE)
file$Total_Price <- as.numeric(file$Total_Price)

file$age_bin[file$AGE < 20] = "Less than 20"
file$age_bin[file$AGE >= 20 & file$AGE < 30] = "20 to 30"
file$age_bin[file$AGE >= 30 & file$AGE < 40] = "30 to 40"
file$age_bin[file$AGE >= 40 & file$AGE < 50] = "40 to 50"
file$age_bin[file$AGE >= 50 & file$AGE < 60] = "50 to 60"
file$age_bin[file$AGE >= 60] = "60 and above"
file$Date <- mdy(file$Date)



#Classifier - Transaction (trend of people who purchase in a store) 

ggplot(file, aes(x = "", y = "Total_Price", fill = Nature_1))+
  geom_bar(width = 1, stat = "identity")+
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5))+
  coord_polar(theta = "y", start=0)+
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Sales"
  )


#classifier - by state

ggplot(file, aes(Month, Total_Price, fill = Nature_1))+
  facet_grid(~CITY, scales = "free_x")+
  stat_summary(fun.y = mean, geom = "bar", position = "dodge")+
  labs(title = "Sales Trend by City", x = "City", y = "Total Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



classifier_city <- function(City){
  
  file_1 <- subset(file, CITY == City)
  write.csv(file_1, file = "text.csv")
  print(table(file_1$CITY))
  
#classifier - by month

p1 <- ggplot(file_1, aes(Month, Total_Price, fill = Nature_1))+
  stat_summary(fun.y = mean, geom = "bar", position = "dodge")+
  labs(title = paste0("Sales Trend by City", " ", file$CITY), x = "City", y = "Total Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

#classifier - by Category

p2 <- ggplot(file_1, aes(CATEGORY, Total_Price, fill = Nature_1))+
  stat_summary(fun.y = mean,geom = "bar", position = "dodge")+
  labs(title = paste0("Sales Trend by Category", " ", file$CITY), x = "Category", y = "Total Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

plot_grid(p1,p2)

}

classifier_city("Bangalore")

#Classifier - product - by state

classfr_city <- function(City, Nature, support, confidence){
  
  file <- file %>% 
    filter(CITY == City)%>%
    filter(Nature_1 == Nature)
  
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
  
  parameters = list(
    support = support,
    confidence = confidence,
    minlen  = 2,  # Minimal number of items per item set
    maxlen  = 10, # Maximal number of items per item set
    target  = "rules")
  
  find.rules <- function(transactions.obj, support, confidence, topN = 10){
    all.rules <- apriori(transactions.obj, parameter = parameters)
    
    rules.df <-data.frame(rules = labels(all.rules)
                          , all.rules@quality)
    
    other.im <- interestMeasure(all.rules, transactions = transactions.obj)
    
    rules.df <- cbind(rules.df, other.im[,c('conviction','leverage')])
    
    
    # Keep the best rule based on the interest measure
    best.rules.df <- head(rules.df[order(-rules.df$leverage),],topN)
    
    return(best.rules.df)
  }
  
  
  cross.sell.rules <- find.rules( transactions.obj, support, confidence )
  cross.sell.rules$rules <- as.character(cross.sell.rules$rules)              
  
  edges <- unlist(lapply(cross.sell.rules['rules'], strsplit, split='=>'))
  
  g <- graph(edges = edges)
  plot(g)  
  
  
}
  
classfr_city("Bangalore", "Family",.01,.60)  
classfr_city("Bangalore", "Individual",.01,.60)  


###################################################################################################################

#Impact on margin

file_margin <- file%>%
  filter(CITY == "Bangalore")

p1 <- ggplot(file_margin, aes(reorder(PDT_CAT_1, Margin,sum), Margin, fill = CATEGORY))+
    stat_summary(fun.y = sum, geom = "bar", position = "dodge")+
  labs(title = "Margin (before Cross Sell)", x = "Product", y = "Margin")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

file_margin_after <- read.csv("Transaction_master_CS.csv")


p2 <- ggplot(file_margin_after, aes(reorder(PDT_CAT_1, Margin,sum), Margin, fill = CATEGORY))+
  stat_summary(fun.y = sum, geom = "bar", position = "dodge")+
  labs(title = "Margin (after cross sell)", x = "Product", y = "Margin")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

plot_grid(p1,p2)


###################################################################################################################











