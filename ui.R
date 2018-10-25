library(shiny)
library(shinydashboard)

########### Header Definition ##################

header <- dashboardHeader(
  title = config$title,
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
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets")
  )
)


############## Body definition #####################

body    <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)


################ Assemble UI ######################

ui <- dashboardPage(
  header,
  sidebar,
  body
)