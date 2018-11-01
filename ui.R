library(shiny)
library(shinydashboard)

########### Header Definition ##################

header <- dashboardHeader(
  title = "Akshi",
  # Messages
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Akshi",
                 message = "Welcome to Akshi."
               )
  ),
  # Notifications
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "2 New Features added today",
                 icon("users")
               )
  )
)

############### Side bar definition #################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sales", tabName = "sales", icon = icon("dashboard")),
    menuItem("Trends", icon = icon("th"), tabName = "trends"),
    menuItem("City", icon = icon("bar-chart-o"), tabName = "city")
    
  )
)


############# YTD Figures #########################
ytd.boxes <-     fluidRow(
   infoBoxOutput("YTDSales")
  ,infoBoxOutput("YTDOrders")
  
)

############### YTD Charts #######################
ytd.figures <- fluidRow(
  
  box(title = "Year to date Sales", status = "primary", solidHeader = TRUE,shiny::plotOutput("YTDSalesValueChart")),
  box(title = "Year to date Quantity sold", status = "primary", solidHeader = TRUE,plotOutput("YTDSalesQtyChart"))
  
)

############ Sales By Week Month ##########################
sales.figures <- fluidRow(
  box(title = "Weekly Sales", status = "primary", solidHeader = TRUE,plotOutput("salesWeekChart")),
  box(title = "Monthly Sales", status = "primary", solidHeader = TRUE,plotOutput("salesMonthChart"))
  
)



############## Sales Trend ##########################

sales.trend <- fluidRow(
  
  box(title = "Sales Trend", status = "primary", solidHeader = TRUE,plotOutput("salesTrend")),
  box(title = "Quantity sold Trend", status = "primary", solidHeader = TRUE,plotOutput("qtyTrend"))
  
)

############# City plots ###########################

city.plots <- box(
  title = "City Sales", status = "primary", solidHeader = TRUE, width='100%',align = 'center',
  fluidRow(
    selectInput(inputId='selectCity', label='Select city', choices=cities, width='90%')			
  ),
  fluidRow(
    plotOutput("citySalesChart", width='90%')
  )
  
)



############## Body definition #####################

body    <- dashboardBody(
  tabItems(
    tabItem(tabName = "sales",
            ytd.boxes,
            ytd.figures, 
            sales.figures
    ),
    
    tabItem(tabName = "trends",
            sales.trend
    ),
    
    tabItem(tabName = "city",
            city.plots)
    
  )
)


################ Assemble UI ######################

ui <- dashboardPage(
  header,
  sidebar,
  body
)