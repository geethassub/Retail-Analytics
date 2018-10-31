

server <- function(input, output) { 
  
  output$YTDSales <- renderInfoBox({
    infoBox("YTD Sales", value = paste(c('INR ', format(ytd.sales, big.mark=",", scientific = FALSE) )
                                       , collapse =  ": "), icon = shiny::icon("credit-card"), fill = TRUE)
  })

  output$YTDOrders <- renderInfoBox({
    infoBox("YTD ORders", value = paste(c('INR ', format(ytd.orders, big.mark=",", scientific = FALSE) )
                                       , collapse =  ": "), icon = icon("th"), fill = TRUE)
  })
  
  
}