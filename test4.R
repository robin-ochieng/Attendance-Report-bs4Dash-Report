# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(bs4Dash)
library(DT) 

# Read the data
Data <- read_excel("data/Data.xlsx", col_types = c("text", 
                                                   "text", "date", "text", "text", "text", 
                                                   "text", "text"))

# Convert 'Time' column to POSIXct format for easier comparison
Data <- Data %>%
  mutate(Time = as.numeric(Time),  # Ensure Time is numeric
         Time = seconds_to_period(Time * 86400),  # Convert to period
         Hour = hour(Time),  # Extract hour component
         Time_of_Day = if_else(hour(Time) > 6 & hour(Time) < 19 |
                                 (hour(Time) == 6 & minute(Time) > 0) |
                                 (hour(Time) == 19 & minute(Time) == 0),
                               "Day", "Night"),  # Determine Time of Day
         Time_Period = case_when(
           Hour >= 4 & Hour < 10 ~ "Morning (6-10)",
           Hour >= 10 & Hour < 12 ~ "Mid-Morning (10-12)",
           Hour >= 12 & Hour < 13 ~ "Noon (12-13)",
           Hour >= 13 & Hour < 16 ~ "Afternoon (13-16)",
           Hour >= 16 & Hour < 19 ~ "Evening (16-19)",
           Hour >= 19 | Hour < 4 ~ "Night (19-00)",  # Adjusted for logical OR as midnight spans from 19 to before 4
           TRUE ~ "Unknown"  # Default case
         ),
         Day_of_week = weekdays(`Date/Time`))  # Extract the day of the week from 'Date/Time'

# Calculating Attendance Metrics
attendance_metrics <- Data %>%
  summarise(
    Total_Sign_In = sum(Status == "Sign In", na.rm = T),
    Total_Sign_Out = sum(Status == "Sign Out", na.rm = T),
    Unique_Days_Counted = n_distinct(`Date/Time`)
  ) %>%
  mutate(
    Sign_In_Frequency = Total_Sign_In / Unique_Days_Counted,
    Sign_In_Rate = Total_Sign_In / (Total_Sign_In + Total_Sign_Out),
    Sign_Out_Frequency = Total_Sign_Out / Unique_Days_Counted,
    Sign_Out_Rate = Total_Sign_Out / (Total_Sign_In + Total_Sign_Out)
  )



# Print the result to check
print(attendance_metrics)

# Now to integrate with your Shiny dashboard using bs4Dash:
# Here's a simplified example of how you might set up the dashboard UI to display these metrics:
ui <- dashboardPage(
  dashboardHeader(title = "Attendance Dashboard",
                  titleWidth = 400),
  dashboardSidebar(
    width = 400,
    tags$li(class = "dropdown-header", style = "color: #fff; background-color: #ffc107; padding: 10px; margin-bottom: 10px;", 
            tags$h3("Navigation Panel", style = "margin-top: 0;")),
    sidebarMenu(
      menuItem("Attendance Metrics", tabName = "metrics", icon = icon("dashboard")),
      menuItem("Data View", tabName = "dataView", icon = icon("table"))
    ),
    tags$div(class = "sidebar-footer", style = "position: absolute; bottom: 0; width: 100%; padding: 10px; text-align: center; background-color: #007bff; color: #fff; font-size: 8px;",
             "Powered by Tech and Research Department",
             br(),
             "Data provided by Kenbright Holdings"
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        body, .content-wrapper, .right-side, .main-footer, .main-header, .sidebar, .control-sidebar {
          font-family: 'Mulish', sans-serif;
        }
        .body-footer {
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
          background-color: #007bff;
          color: white;
          text-align: center;
          padding: 10px;
          z-index: 1030;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "metrics",
              fluidRow(
                valueBoxOutput("totalSignIn", width = 3),
                valueBoxOutput("totalSignOut", width = 3),
                valueBoxOutput("uniqueDaysCounted", width = 3),
                valueBoxOutput("AverageDailySignIns", width = 3),
                valueBoxOutput("AverageDailySignOuts", width = 3),
                valueBoxOutput("signInRate", width = 3),
                valueBoxOutput("signOutRate", width = 3)
              )
      ),
      tabItem(tabName = "dataView",
              box(title = "Attendance Data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                  DTOutput("dataTable", width = "100%")))
      ),
    div(class = "body-footer", "© 2024 Dashboard - All Rights Reserved")  # Body Footer
  ),
  title = "Dashboard",
  skin = "blue",
  controlbar = dashboardControlbar(  # Adding a control bar for extra settings or information
    controlbarMenu(
      id = "controlbar_menu",
      controlbarItem("Adjustments", icon = icon("tools"),
                     p("Sidebar width:"),
                     sliderInput("sidebar_width", "Width", min = 200, max = 500, value = 300)
      )
    )
  )
)
server <- function(input, output) {
  output$dataTable <- renderDT({
    datatable(Data,
              options = list(
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#007bff', 'color': '#ffffff'});",
                "}"
              ),
              dom = 'Bfrtip',  # Allows for buttons, searching, pagination
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Export options
              autoWidth = TRUE,
              responsive = TRUE,
              paging = TRUE,
              scrollX = TRUE,
              searchHighlight = TRUE  # Highlights search terms
    ))
  }, server = FALSE)  # Non-server side processing for interactive features
  
  observeEvent(input$toggleData, {
    shinyjs::toggle(id = "dataTable")
  })
  
  
  output$totalSignIn <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_In,
      subtitle = "Total Sign Ins",
      icon = icon("sign-in-alt"),
      color = "primary"
    )
  })
  output$totalSignOut <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_Out,
      subtitle = "Total Sign Outs",
      icon = icon("sign-out-alt"),
      color = "warning"
    )
  })
  output$uniqueDaysCounted <- renderValueBox({
    valueBox(
      attendance_metrics$Unique_Days_Counted,
      subtitle = "Unique Days Counted",
      icon = icon("calendar"),
      color = "info"
    )
  })
  output$AverageDailySignIns <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_In_Frequency),
      subtitle = "Average Daily Sign Ins",
      icon = icon("sign-in-alt"),
      color = "success"
    )
  })
  output$signInRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_In_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent"),
      color = "danger"
    )
  })
  output$AverageDailySignOuts <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_Out_Frequency),
      subtitle = "Average Daily Sign Outs",
      icon = icon("sign-out-alt"),
      color = "success"
    )
  })
  output$signOutRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_Out_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent"),
      color = "danger"
    )
  })
}

shinyApp(ui, server)
