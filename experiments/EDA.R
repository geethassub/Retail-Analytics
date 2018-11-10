library(ggplot2)
library(scales)
library(dplyr)
library(cowplot)
library(lubridate)
setwd("C:/Geetha/Ondipuli/RMKV")

file <-  read.csv("Transaction.csv")
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


#Sales Insight - overall analysis for all states

#Compare sales at multiple locations - by month/category/customer/

#ggplot(file, aes(x = CITY, y = Total_Price, fill = CATEGORY))+
#stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
#labs(title = "City Wise sale details", x = "City", y = "Average Sales")+
#geom_text(aes(label=scales::percent(..prop../sum(..prop..))), position=position_dodge(width=0.9), vjust=-0.25)

#Overall sales by the company

ggplot(file, aes(x = "", y = "Total_Price", fill = Month))+
  geom_bar(width = 1, stat = "identity")+
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5))+
  coord_polar(theta = "y", start=0)+
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Sales"
  )


###################################################################################
#Overall sales plots

p1 <- ggplot(file, aes(Month, Total_Price, fill = CITY))+
  geom_bar(stat = "identity")+
  labs(title = "Sales by Value- 2017", x = "Month", y = "Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CITY)

p2 <- ggplot(file, aes(Month, QUANTITY, fill = CITY))+
  geom_bar(stat = "identity")+
  labs(title = "Sales by quantity - 2017", x = "Month", y = "Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CITY)

plot_grid(p1,p2)

#################################################################################

#Sales by week/Month

p3 <- ggplot(file, aes(x = Day_1, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details (Day and Gender)", x = "Day of the week", y = "Average Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)

p4 <- ggplot(file, aes(x = Month, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details(Month and Gender)", x = "Month", y = "Average Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)


p5 <- ggplot(file, aes(x = Day_2, y = Total_Price, fill = CITY))+
  stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
  labs(title = "City Wise sale details (WeekDay/Weekend and Gender)", x = "Week", y = "Average Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(~CUST_SEX)

plot_grid(p3,p4,p5)

####################################################################################

#sales analysis by category
ggplot(file, aes(x = PDT_CAT_1, y = Total_Price, fill = age_bin))+
  facet_grid(~CATEGORY,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Category Wise sales", x = "Category", y = "Average Sales")+
  stat_summary(fun.y = "mean", geom = "bar", position = position_dodge(.9))

####################################################################################


#Trend in sales

ggplot(file, aes(x = Month, y = Total_Price, group = 1))+
  facet_wrap(~ CITY) + theme(legend.position = "none") + 
  stat_summary(fun.y="mean", geom="line") + 
  geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Trend in Sales", x = "Month", y = "Average Sales")

ggplot(file, aes(x = Month, y = QUANTITY, group = 1))+
  facet_wrap(~ CITY) + theme(legend.position = "none") + 
  stat_summary(fun.y="mean", geom="line") + 
  geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Trend in Sales (Quantity)", x = "Month", y = "Average Quantity")


#ggplot(file, aes(x = CITY, y = Total_Price, fill = CATEGORY))+
#geom_bar(stat = "identity")+
#geom_text(aes(label=scales::percent(..prop../sum(..prop..))), position=position_dodge(width=0.9), vjust=-0.25)

##################################################################################

#Build a model for a city 

city_graph <- function(City){
  
  file <- file %>% 
    filter(CITY == City)
  
  
  
  p1 <- ggplot(data = file, aes(x = Day_1, y = Total_Price, fill = Month))+
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Average Sales Day Wise"," ", file$CITY))+
    facet_grid(~CUST_SEX)
  
  
  p2 <- ggplot(data = file, aes(x = Month, y = Total_Price, fill = CATEGORY))+
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Average Sales Category Wise", " ", file$CITY))+
    facet_grid(~CUST_SEX)
  
  p3 <- ggplot(file, aes(x = PDT_CAT_1, y = Total_Price, fill = age_bin))+
    facet_grid(~CATEGORY,scales = "free_x")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Category Wise sales" , " ", file$CITY), x = "Category", y = "Average Sales")+
    stat_summary(fun.y = "mean", geom = "bar", position = position_dodge(.9))
  
  
  p4 <- ggplot(file, aes(x = Month, y = Total_Price, group = 1, color = CATEGORY))+
    facet_wrap(~ CATEGORY) + theme(legend.position = "none") + 
    stat_summary(fun.y="mean", geom="line") + 
    geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Trend in Sales", " ", file$CITY))
  
  p5 <- ggplot(file, aes(x = Month, y = QUANTITY, group = 1))+
    facet_wrap(~ CATEGORY) + theme(legend.position = "none") + 
    stat_summary(fun.y="mean", geom="line") + 
    geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Trend in Sales (Quantity)", " ", file$CITY), x = "Month", y = "Average Quantity")
  
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  
}

city_graph("Bangalore")


#forecast sales 
library(forecast)
library(zoo)
file$age_bin <- NULL
file_1 <- ts(file)

fc <- forecast(file$Total_Price)
autoplot(fc)+geom_forecast(h =30)











