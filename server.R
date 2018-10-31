
library(ggplot2)

server <- function(input, output) { 
  
  output$YTDSales <- renderInfoBox({
    infoBox("YTD Sales", value = paste(c('INR ', format(ytd.sales, big.mark=",", scientific = FALSE) )
                                       , collapse =  ": "), icon = shiny::icon("credit-card"), fill = TRUE)
  })

  output$YTDOrders <- renderInfoBox({
    infoBox("YTD ORders", value = paste(c('INR ', format(ytd.orders, big.mark=",", scientific = FALSE) )
                                       , collapse =  ": "), icon = icon("th"), fill = TRUE)
  })
  
  output$YTDSalesChart <- renderPlot(
    ggplot(txn.data, aes(x = "", y = "Total_Price", fill = Month))+
      geom_bar(width = 1, stat = "identity")+
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5))+
      coord_polar(theta = "y", start=0)+
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of Sales"
      )
  )
  
  output$YTDSalesValueChart <- renderPlot(
    
    ggplot(txn.data, aes(Month, Total_Price, fill = CITY))+
      geom_bar(stat = "identity")+
      labs(title = "Sales by Value- 2017", x = "Month", y = "Sales")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CITY)

  )
  
  output$YTDSalesQtyChart <- renderPlot(
    
    ggplot(txn.data, aes(Month, QUANTITY, fill = CITY))+
      geom_bar(stat = "identity")+
      labs(title = "Sales by quantity - 2017", x = "Month", y = "Sales")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CITY)
    
  )
  
  output$salesWeekChart <- renderPlot(
    
    ggplot(txn.data, aes(x = Day_1, y = Total_Price, fill = CITY))+
      stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
      labs(title = "City Wise sale details (BY Gender)", x = "Day of the week", y = "Average Sales")+
      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CUST_SEX)
  )
  
  output$salesMonthChart <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = Total_Price, fill = CITY))+
      stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
      labs(title = "City Wise sale details(By Gender)", x = "Month", y = "Average Sales")+
      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CUST_SEX)
  )
  
  output$salesTrend <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = Total_Price, group = 1))+
      facet_wrap(~ CITY) + theme(legend.position = "none") + 
      stat_summary(fun.y="mean", geom="line") + 
      geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
      theme(axis.text.x = element_text(angle = 90))+
      labs(title = "Trend in Sales", x = "Month", y = "Average Sales")
  )
  
  output$qtyTrend <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = QUANTITY, group = 1))+
      facet_wrap(~ CITY) + theme(legend.position = "none") + 
      stat_summary(fun.y="mean", geom="line") + 
      geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
      theme(axis.text.x = element_text(angle = 90))+
      labs(title = "Trend in Sales (Quantity)", x = "Month", y = "Average Quantity")
  )
  
}