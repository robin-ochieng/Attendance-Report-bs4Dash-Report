# Load necessary libraries
library(readxl)
library(readxl)
library(dplyr)
library(lubridate)

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
           Hour >= 6 & Hour < 10 ~ "Morning (6-10)",
           Hour >= 10 & Hour < 12 ~ "Mid-Morning (10-12)",
           Hour >= 12 & Hour < 13 ~ "Noon (12-13)",
           Hour >= 13 & Hour < 16 ~ "Afternoon (13-16)",
           Hour >= 16 & Hour < 19 ~ "Evening (16-19)",
           Hour >= 19 | Hour < 6 ~ "Night (19-6)",  # Adjusted for logical OR as midnight spans from 19 to before 4
           TRUE ~ "Unknown"  # Default case
         ),
         Day_of_week = weekdays(`Date/Time`),  # Extract the day of the week from 'Date/Time'
         Division_Name = paste(Division, Name, sep = " - "),  # Combine 'Division' and 'Name'
         Time_str = sprintf("%02d:%02d:%02d", as.integer(hour(Time)), as.integer(minute(Time)), as.integer(second(Time))),
         FirstSignInTime = as.POSIXct(paste(`Date/Time`, Time_str), format = "%Y-%m-%d %H:%M:%S"),
         Time1 = ifelse(is.na(Time) | Time < 0 | Time > 1, NA, Time),
         Time2 = ifelse(is.na(Time), NA, format(as.POSIXct(Time * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"))
  )



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

# Summarize to find the first sign-in time for each combination of Date/Time, Name, and Division
EarlySignInDetails <- Data %>%
  filter(Status == "Sign In") %>%
  group_by(`Date/Time`, Name, Division) %>%
  summarise(FirstSignInTime = min(FirstSignInTime), .groups = 'drop') %>%
  filter(FirstSignInTime < as.POSIXct(paste(`Date/Time`, "08:15:00"), format = "%Y-%m-%d %H:%M:%S"))

# Create the LateSignInDetails table
LateSignInDetails <- Data %>%
  filter(Status == "Sign In") %>%  # Filter only sign-in records
  group_by(`Date/Time`, Name, Division) %>%  # Group by Date/Time, Name, and Division
  summarise(FirstSignInTime = min(FirstSignInTime), .groups = "drop") %>%  # Calculate minimum sign-in time for each group
  filter(FirstSignInTime > as.POSIXct(paste(`Date/Time`, "08:16:00"), format = "%Y-%m-%d %H:%M:%S"))  # Filter for late sign-ins after 8:15 AM




# Create the GroupedTable
GroupedTable <- Data %>%
  group_by(`Date/Time`, Name, Division, Day_of_week) %>%
  summarise(
    `Earliest sign in Time` = if (all(is.na(Time1[Status == "Sign In"]))) NA_real_ else min(Time1[Status == "Sign In"], na.rm = TRUE),
    `Latest sign Out Time` = if (all(is.na(Time1[Status == "Sign Out"]))) NA_real_ else max(Time1[Status == "Sign Out"], na.rm = TRUE),
    .groups = 'drop'  # Remove grouping structure after summarisation
  )


# Check the result
View(LateSignInDetails)
View(GroupedTable)
View(EarlySignInDetails)
View(Data)
View(attendance_metrics)
