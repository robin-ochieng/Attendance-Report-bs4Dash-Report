# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(bs4Dash)

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
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("totalSignIn"),
      valueBoxOutput("totalSignOut"),
      valueBoxOutput("uniqueDaysCounted"),
      valueBoxOutput("AverageDailySignIns"),
      valueBoxOutput("AverageDailySignOuts"),
      valueBoxOutput("signInRate"),
      valueBoxOutput("signOutRate")
    )
  )
)

server <- function(input, output) {
  output$totalSignIn <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_In,
      subtitle = "Total Sign Ins",
      icon = icon("sign-in-alt")
    )
  })
  output$totalSignOut <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_Out,
      subtitle = "Total Sign Outs",
      icon = icon("sign-out-alt")
    )
  })
  output$uniqueDaysCounted <- renderValueBox({
    valueBox(
      attendance_metrics$Unique_Days_Counted,
      subtitle = "Unique Days Counted",
      icon = icon("calendar")
    )
  })
  output$AverageDailySignIns <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_In_Frequency),
      subtitle = "Average Daily Sign Ins",
      icon = icon("sign-in-alt")
    )
  })
  output$signInRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_In_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent")
    )
  })
  output$AverageDailySignOuts <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_Out_Frequency),
      subtitle = "Average Daily Sign Outs",
      icon = icon("sign-out-alt")
    )
  })
  output$signOutRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_Out_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent")
    )
  })
}

shinyApp(ui, server)
