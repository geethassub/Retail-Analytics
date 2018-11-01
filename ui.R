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
    menuItem("City", icon = icon("bar-chart-o"), tabName = "city"),
    menuItem("xsell", icon = icon("anchor"), tabName = "xsell")
    
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

############ Xsell ##############################

xsell.panels.config <- box(
  title = "Thresholds", status = "primary", solidHeader = TRUE, width = '100%', align='center',
  fluidRow(
    sliderInput("Support", "Support:", min = 0.01, max = 1.0, value = 0.01, width = '90%'),
    sliderInput("Confidence", "Confidence:", min = 0.05, max = 1.0, value = 0.05, width='90%')
  )
)

xsell.panels <- box(
  title = "Rules", status = "primary", solidHeader = TRUE, width = '100%', align='center',
  fluidRow(
    column(width = 12, align = 'center',
    tabsetPanel(
      id = 'xsell',
      tabPanel('Rules',  DT::dataTableOutput('rulesTable')),
      tabPanel('Explore', plotOutput('explorePlot')),
      tabPanel('Item Groups',  plotOutput('graphPlot'))
    )
    )
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
            city.plots
    ),
    tabItem(tabName = "xsell",
            xsell.panels.config,
            xsell.panels
            )
    
  )
)


################ Assemble UI ######################

ui <- dashboardPage(
  header,
  sidebar,
  body
)