library(ggplot2)
library(scales)
library(dplyr)
library(cowplot)
setwd("C:/Geetha/Ondipuli/RMKV")

file <-  read.csv("Transaction.csv")
names(file)


#Sales Insight - overall analysis for all states

#Compare sales at multiple locations - by month/category/customer/

#ggplot(file, aes(x = CITY, y = Total_Price, fill = CATEGORY))+
    #stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
    #labs(title = "City Wise sale details", x = "City", y = "Average Sales")+
    #geom_text(aes(label=scales::percent(..prop../sum(..prop..))), position=position_dodge(width=0.9), vjust=-0.25)

  
ggplot(file, aes(x = Day_1, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details", x = "Day of the week", y = "Average Sales")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)

ggplot(file, aes(x = Month, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details", x = "Month", y = "Average Sales")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)


ggplot(file, aes(x = Day_2, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details", x = "Week", y = "Average Sales")+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)

ggplot(file, aes(x = PDT_CAT_1, y = Total_Price))+
  facet_grid(~CATEGORY,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Category Wise sales", x = "Category", y = "Average Sales")+
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(file, aes(x = Date, y = Total_Price, colour = CATEGORY,group = CATEGORY))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Trend in Sales")

ggplot(file, aes(x = Date, y = QUANTITY, colour = CATEGORY,group = CATEGORY))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Trend in Quantity sold")+
  facet_grid(~CUST_SEX)


ggplot(file, aes(x = CATEGORY, fill = Month))+
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge") + 
  scale_y_continuous(labels=percent)+
  labs(title = "Trend of selling products by Category", y = "Sales")




#ggplot(file, aes(x = CITY, y = Total_Price, fill = CATEGORY))+
  #geom_bar(stat = "identity")+
  #geom_text(aes(label=scales::percent(..prop../sum(..prop..))), position=position_dodge(width=0.9), vjust=-0.25)




#Build a model for a city 

city_graph <- function(City){
  
  file %>% 
  filter(CITY == City)
  
  
  
  p1 <- ggplot(data = file, aes(x = Day_1, y = Total_Price, fill = Month))+
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = "Average Sales Day Wise")+
    facet_grid(~CUST_SEX)
  
  
  p2 <- ggplot(data = file, aes(x = Day_2, y = Total_Price, fill = CATEGORY))+
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = "Average Sales Category Wise")+
    facet_grid(~CUST_SEX)
  
  p3 <- ggplot(file, aes(x = Date, y = Total_Price, colour = CATEGORY,group = CATEGORY))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = "Trend in Sales")+
    facet_grid(~CUST_SEX)
  
  p4 <- ggplot(file, aes(x = Date, y = QUANTITY, colour = CATEGORY,group = CATEGORY))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = "Trend in Quantity sold")+
    facet_grid(~CUST_SEX)
  
  
    
  plot_grid(
    
    p4 <- ggplot(file, aes(x = Date, y = Total_Price))+
          geom_line()
  )
  
  
  
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
}

city_graph("Bangalore")


#Identify trend in sales
















