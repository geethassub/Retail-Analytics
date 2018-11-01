
library(ggplot2)
library(DT)

server <- function(input, output) { 
  
  
  react.data <- reactiveValues()
  
  observe({
  
    selected.city <- input$selectCity
    react.data$city.data <- txn.data[txn.data$CITY == selected.city,]
      
  })
  
  output$citySalesChart <- renderPlot(
    
    ggplot(data = react.data$city.data, aes(x = Day_1, y = Total_Price, fill = Month))+
      stat_summary(fun.y = "mean", geom = "bar", position = "dodge")+
      theme(axis.text.x = element_text(angle = 90))+ theme_minimal() +
      labs(title = paste0("Average Sales Day Wise"," ", input$selectCity))+
      facet_grid(~CUST_SEX)
    
  )
  
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
      theme_minimal() +      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CITY)

  )
  
  output$YTDSalesQtyChart <- renderPlot(
    
    ggplot(txn.data, aes(Month, QUANTITY, fill = CITY))+
      geom_bar(stat = "identity")+
      labs(title = "Sales by quantity - 2017", x = "Month", y = "Sales")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))+
      facet_grid(~CITY)
    
  )
  
  output$salesWeekChart <- renderPlot(
    
    ggplot(txn.data, aes(x = Day_1, y = Total_Price, fill = CITY))+
      stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
      labs(title = "City Wise sale details (BY Gender)", x = "Day of the week", y = "Average Sales")+
      theme(axis.text.x = element_text(angle = 90))+ theme_minimal() +
      facet_grid(~CUST_SEX)
  )
  
  output$salesMonthChart <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = Total_Price, fill = CITY))+
      stat_summary(fun.y = "mean", geom = 'bar', position = "dodge")+
      labs(title = "City Wise sale details(By Gender)", x = "Month", y = "Average Sales")+
      theme(axis.text.x = element_text(angle = 90))+ theme_minimal() +
      facet_grid(~CUST_SEX)
  )
  
  output$salesTrend <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = Total_Price, group = 1))+
      facet_wrap(~ CITY) + theme(legend.position = "none") + 
      stat_summary(fun.y="mean", geom="line") + 
      geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
      theme(axis.text.x = element_text(angle = 90))+ theme_minimal() +
      labs(title = "Trend in Sales", x = "Month", y = "Average Sales")
  )
  
  output$qtyTrend <- renderPlot(
    ggplot(txn.data, aes(x = Month, y = QUANTITY, group = 1))+
      facet_wrap(~ CITY) + theme(legend.position = "none") + 
      stat_summary(fun.y="mean", geom="line") + 
      geom_smooth(method="lm", se=FALSE, linetype=3, color="black")+
      theme(axis.text.x = element_text(angle = 90))+ theme_minimal() +
      labs(title = "Trend in Sales (Quantity)", x = "Month", y = "Average Quantity")
  )
  
  
  cross.sell.rules <- reactive({
    support <- input$Support
    confidence <- input$Confidence
    cross.sell.rules <- find.rules( transactions.obj, support, confidence )
    cross.sell.rules$rules <- as.character(cross.sell.rules$rules)              
    return(cross.sell.rules)
    
  })
  
  gen.rules <- reactive({
    support <- input$Support
    confidence <- input$Confidence
    gen.rules <- get.rules(  support, confidence ,transactions.obj)
    return(gen.rules)
    
  })
  
  
  output$rulesTable <- DT::renderDataTable({
    DT::datatable(
    cross.sell.rules(),options = list(pageLength = 10,lengthChange=FALSE))%>%
      formatRound(c(3:9), 2) %>% 
      formatStyle(columns = c(3:9), 'text-align' = 'center')
  })
  output$graphPlot <- renderPlot({
    g <-plot.graph(cross.sell.rules())
    plot(g)
  })
  
  output$explorePlot <- renderPlot({
    plot(x = gen.rules(), method = NULL, 
         measure = "support", 
         shading = "lift", interactive = FALSE)
  })
  
  
  
}