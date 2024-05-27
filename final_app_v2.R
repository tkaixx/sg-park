library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(reshape2)
library(sf)
library(tidyr) 
library(httr)
library(jsonlite)

# LOTS AVAILABILITY TAB DATA - MALLS & HDB

# Load data from CSV files
mall_carpark_data <- read.csv("./mall_carpark.csv", check.names = FALSE)
hdb_carpark_data <- read.csv("./parking_codes.csv", check.names = FALSE)


# Exclude carparks from the mall data
carparks_to_exclude_mall <- c('Mandarin Hotel', '[CapitaLand] Capital Tower', 'JCube', 'The Star Vista')
mall_carpark_data <- mall_carpark_data[!(mall_carpark_data$Carpark %in% carparks_to_exclude_mall), ]

day_of_the_week <- function(mall_carpark_data, target_day) {
  # Exclude carparks with negative consistent values and those with the same lots availability throughout the entire period from 5am to 12am
  carparks_to_exclude <- c('Mandarin Hotel', '[CapitaLand] Capital Tower', 'JCube', 'The Star Vista')
  mall_carpark_data <- mall_carpark_data[!(mall_carpark_data$Carpark %in% carparks_to_exclude), ]
  
  # Replace negative values with 0
  mall_carpark_data <- mall_carpark_data %>%
    mutate(across(-Carpark, ~ifelse(. < 0, 0, .)))
  
  # Extract the columns with the specified day
  selected_cols <- c("Carpark", grep(tolower(target_day), tolower(colnames(mall_carpark_data)), value = TRUE))
  selected_data <- mall_carpark_data[, selected_cols, drop = FALSE]
  colnames(selected_data) <- c("Carpark", gsub(target_day, "", colnames(selected_data)[-1]))
  colnames(selected_data)[-1] <- gsub("_", "", colnames(selected_data)[-1])  # Update column names to remove "_"
  
  return(selected_data)
}

# Exclude specified carparks from the HDB data
carparks_to_exclude_hdb <- c('D0026', 'J0122', 'A0007', 'B0031', 'E0024', 'A0017', 'C0119', 'L0104', 'P0093',
                             'B0063', 'C0162', 'K0082', 'L0116', 'M0084', 'N0012', 'N0013', 'O0028', 'P0013',
                             'M0040', 'M0076', 'P0048', 'Y0019', 'J0054', 'M0059', 'P0109', 'S0108', 'E0023', 'J0092',
                             'L0078', 'T0103', 'C0133', 'E0027', 'K0111', 'L0116', 'L0123', 'P0075', 'P0106', 'Q0006', 'B0088',
                             'M0088', 'S0106', 'H0022', 'T0129', 'S0020','T0017','Q0008')
hdb_carpark_data <- hdb_carpark_data[!(hdb_carpark_data$Carpark %in% carparks_to_exclude_hdb), ]

# Extract unique mall and HDB names
mall_carpark_name <- unique(mall_carpark_data$Carpark)
hdb_carpark_name <- unique(hdb_carpark_data$Carpark)

# GO WHERE TAB DATA
df_hawker <- read.csv("HawkerCentresKML.csv")
df_mall <- read.csv("mall_coordinates_updated.csv")

df_mall_carparks <- read.csv("mall_carpark.csv")
df_mall$name <- tolower(df_mall$name)
# Remove the [CapitalLand] prefix from the Carpark names
df_mall_carparks$Carpark2 <- gsub("\\[CapitaLand\\]\\s?", "", df_mall_carparks$Carpark)
df_mall_carparks$Carpark2 <- tolower(df_mall_carparks$Carpark2)

# Now filter df_mall to only include names that are present in df_mall_carparks
df_mall_carparks <- df_mall[df_mall$name %in% df_mall_carparks$Carpark2, ]

df_mallprice <- read.csv("mall_price_cleaned.csv")
df_mallprice <- df_mallprice[ , -1]
df_hdbprice <- read.csv("hdb_price_avail.csv")
df_hdbprice <- df_hdbprice[, -1]

df_hdblots <- read.csv("hdb_lots_avail.csv") #not real-time, can call realtime hdb lots via api in another R file 

df_mallprice$Car.Park <- tolower(df_mallprice$Car.Park)
mall_names <- unique(df_mall_carparks$name)
combined_names <- c(mall_names, unique(df_hdblots$Result.carparkNo))

###NEW
df_hdb2 <- df_hdblots %>%
  rename(name = Result.carparkNo)

# Rename the columns in df2 to match those of df1
df_mall2 <- df_mall_carparks %>%
  rename(long = longitude, lat = latitude)

# Combine the two dataframes
combined_df <- bind_rows(df_hdb2[,c(4,9,10)], df_mall2[,c(2,3,4)])
combined_df
####

#df_park
library(sf)
geojson_file <- "Parks.geojson"
# Read the GeoJSON file
geojson_data <- st_read(geojson_file)

# Display the structure of the data
print(geojson_data)
geojson_data %>% View()


df_park <- cbind(geojson_data, st_coordinates(geojson_data$geometry))

# Rename columns
colnames(df_park)[c(2,3,4)] <- c('details','lon','lat')



#function for pricing - PARKING RATES 

# Function to calculate the price based on time difference
calculate_price_breakdown <- function(datetime1, datetime2) {
  # Define your pricing rules based on days and hours
  weekday_rate <- 1.2  # Replace with your actual weekday rate
  weekend_rate <- 1.5  # Replace with your actual weekend rate
  peak_hour_rate <- 1.2  # Replace with your actual peak hour rate
  off_peak_hour_rate <- 1.0  # Replace with your actual off-peak hour rate
  
  # Initialize total price and calculate the time difference
  total_price <- 0
  time_difference <- difftime(datetime2, datetime1, units = "hours")
  
  # Split the time difference into individual days
  days <- seq.Date(as.Date(datetime1), as.Date(datetime2), by = "days")
  
  # Calculate the price for each day
  for (day in days) {
    # Check if the day is a weekday or weekend
    if (weekdays(day) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
      # Weekday pricing
      daily_rate <- weekday_rate
    } else {
      # Weekend pricing
      daily_rate <- weekend_rate
    }
    
    # Calculate the start and end of the day
    day_start <- max(datetime1, as.POSIXct(paste(day, "00:00:00")))
    day_end <- min(datetime2, as.POSIXct(paste(day, "23:59:59")))
    
    # Calculate the time difference for the day
    daily_time_difference <- as.numeric(difftime(day_end, day_start, units = "hours"))
    
    # Check if the day includes peak hours
    if (hour(day_start) < 17 & hour(day_end) >= 9) {
      # Apply peak hour pricing
      daily_rate <- daily_rate * peak_hour_rate
    }
    
    # Calculate the daily price
    daily_price <- daily_time_difference * daily_rate
    
    # Add the daily price to the total price
    total_price <- total_price + daily_price
  }
  
  return(total_price)
}


#Calculations
find_carpark_rates <- function(carpark_name, df) {
  flat_fees <- c(
    weekday_rate2 = 0,
    saturday_rate = 0,
    sunday_rate = 0
  )
  
  # Define rates per half-hour
  first_hour_rates <- c(
    weekday_rate1 = 0,
    weekday_rate2 = 0,
    saturday_rate = 0,
    sunday_rate = 0
  )
  
  subsequent_half_hour_rates <- c(
    weekday_rate1 = 0,
    weekday_rate2 = 0,
    saturday_rate = 0,
    sunday_rate = 0
  )
  
  row_index <- which(df$Car.Park == carpark_name)
  if (length(row_index) == 0) {
    row_index <- which(df$Result.ppCode == carpark_name)
  } else {
    row_index <- row_index[1]
  }
  if(row_index == 0)
    #Weekday Rate 1
    if (is.na(df[row_index,6]) == FALSE) {
      first_hour_rates["weekday_rate1"] <- df[row_index,6]
    }
  
  if (is.na(df[row_index,7]) == FALSE) {
    subsequent_half_hour_rates["weekday_rate1"] <- df[row_index,7]
  }
  
  #Weekday rate 2
  if (is.na(df[row_index,8]) == FALSE & is.na(df[row_index,9]) == FALSE & is.na(df[row_index,10]) == FALSE) {
    first_hour_rates["weekday_rate2"] <- df[row_index,6]
    subsequent_half_hour_rates["weekday_rate2"] <- df[row_index,7]
  } else 
  {
    if (is.na(df[row_index,8]) == FALSE) {
      first_hour_rates["weekday_rate2"] <- df[row_index,8]
    }
    if (is.na(df[row_index,9]) == FALSE) {
      subsequent_half_hour_rates["weekday_rate2"] <- df[row_index,9]
    }
    if (is.na(df[row_index,10]) == FALSE) {
      flat_fees["weekday_rate2"] <- df[row_index,10]
    }
  }
  
  #Saturday
  if (is.na(df[row_index,11]) == TRUE && is.na(df[row_index,12]) == TRUE && is.na(df[row_index,13]) == TRUE) {
    same_rate_saturday <- TRUE
  } else 
  {
    if (is.na(df[row_index,11]) == FALSE) {
      first_hour_rates["saturday_rate"] <- df[row_index,11]
    }
    if (is.na(df[row_index,12]) == FALSE) {
      subsequent_half_hour_rates["saturday_rate"] <- df[row_index,12]
    }
    if (is.na(df[row_index,13]) == FALSE) {
      flat_fees["saturday_rate"] <- df[row_index,13]
    }
    same_rate_saturday <- FALSE
  }
  
  #Sunday
  if (is.na(df[row_index,14]) == TRUE && is.na(df[row_index,15]) == TRUE && is.na(df[row_index,16]) == TRUE) {
    same_rate_sunday <- TRUE
  } else 
  {
    if (is.na(df[row_index,14]) == FALSE) {
      first_hour_rates["sunday_rate"] <- df[row_index,14]
    }
    if (is.na(df[row_index,15]) == FALSE) {
      subsequent_half_hour_rates["sunday_rate"] <- df[row_index,15]
    }
    if (is.na(df[row_index,16]) == FALSE) {
      flat_fees["sunday_rate"] <- df[row_index,16]
    }
    same_rate_sunday <- FALSE
  }
  
  return(list(flat_fees = flat_fees, first_hour_rates = first_hour_rates, subsequent_half_hour_rates = subsequent_half_hour_rates, same_rate_saturday, same_rate_sunday))
}

# Function to calculate parking cost based on half-hours with different rates for weekdays, Saturday, and Sunday,
# and flat fees, first hour, and subsequent half-hour rates for weekday rate 1 and weekday rate 2
calculate_parking_cost <- function(start_time, end_time, rates_data, same_rate_saturday = FALSE, same_rate_sunday = FALSE) {
  # Define flat fees and rates
  flat_fees <- rates_data[1][[1]]
  
  # Define rates per half-hour
  first_hour_rates <- rates_data[2][[1]]
  
  subsequent_half_hour_rates <- rates_data[3][[1]]
  
  # Initialize total cost
  total_cost <- 0.0
  
  # Loop through each half-hour
  current_time <- start_time
  while (current_time < end_time) {
    hour_of_day <- as.POSIXlt(current_time)$hour
    day_of_week <- weekdays(current_time)
    
    # Determine the rate for the current half-hour
    if ((hour_of_day >= 7 && hour_of_day < 18) && !grepl("Saturday|Sunday", day_of_week)) {
      flat_fee <- 0.0  # No flat fee for weekday rate 1
      first_hour_rate <- first_hour_rates["weekday_rate1"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["weekday_rate1"]
    } else if ((hour_of_day >= 18 || hour_of_day < 7) && !grepl("Saturday|Sunday", day_of_week)) {
      flat_fee <- flat_fees["weekday_rate2"]
      first_hour_rate <- first_hour_rates["weekday_rate2"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["weekday_rate2"]
    } else if (day_of_week == "Saturday" && (same_rate_saturday || same_rate_sunday)) {
      flat_fee <- 0.0  # No flat fee for weekday rate 1 on Saturday and Sunday
      first_hour_rate <- first_hour_rates["weekday_rate1"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["weekday_rate1"]
    } else if (day_of_week == "Saturday") {
      flat_fee <- flat_fees["saturday_rate"]
      first_hour_rate <- first_hour_rates["saturday_rate"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["saturday_rate"]
    } else if (day_of_week == "Sunday" && same_rate_sunday) {
      flat_fee <- 0.0  # No flat fee for weekday rate 1 on Saturday and Sunday
      first_hour_rate <- first_hour_rates["weekday_rate1"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["weekday_rate1"]
    } else if (day_of_week == "Sunday") {
      flat_fee <- flat_fees["sunday_rate"]
      first_hour_rate <- first_hour_rates["sunday_rate"]
      subsequent_half_hour_rate <- subsequent_half_hour_rates["sunday_rate"]
    }
    
    # Add the flat fee once per entry
    if (total_cost == 0) {
      total_cost <- total_cost + flat_fee
    }
    
    # Determine the rate for the current half-hour
    if (hour_of_day == as.POSIXlt(start_time)$hour && as.POSIXlt(current_time)$min >= as.POSIXlt(start_time)$min) {
      total_cost <- total_cost + first_hour_rate[[1]] / 2
    } else {
      total_cost <- total_cost + subsequent_half_hour_rate[[1]]
    }
    
    # Move to the next half-hour
    current_time <- current_time + 1800  # 1800 seconds = 30 minutes
    
    # Stop the loop if the current time exceeds the end time
    if (current_time >= end_time) {
      break
    }
  }
  
  if(total_cost != 0){
    return(round(total_cost, 2))
  }
  else
  {return("No pricing data")}
}


# UI
ui <- navbarPage("SGPark", # navbar - multiple distinct subcomponents 
                 tabPanel("About SGPark", # 1st navigation tab
                          # Add SGPark logo image, title 1, app description, and title 2
                          tags$div(
                            style = "display: flex; align-items: flex-start;",
                            img(src = "SGPark_Logo.jpeg", height = "400px", style = "margin-right: 20px;"),
                            tags$div(
                              tags$h1("SGPark - Your Gateway to Hassle-Free Journeys in Singapore", style = "font-size: 32px; font-weight: bold;"),
                              tags$p("Welcome to SGPark, the revolutionary mobile application designed exclusively for drivers navigating Singapore's vibrant streets. 
                                     Our mission is to make your journeys stress-free by providing real-time information to plan your trips efficiently. 
                                     Discover nearby hawker centers, parks, and find parking facilities with ease, all while enjoying forecasts on parking lot availability. 
                                     SGPark ensures parking has never been easier – have all the information you need at your fingertips. 
                                     Say goodbye to the frustration of searching for parking spaces and embrace a smoother, more enjoyable ride with SGPark. 
                                     Your ultimate companion for a seamless experience on the roads.", 
                                     style = "font-size: 18px; margin-bottom: 50px;"),
                              tags$h2("Features", style = "font-size: 30px; font-weight: bold;"),
                              tags$ul(
                                tags$li("1. Real-time Parking Lot Predictions:",
                                        style = "font-weight: bold; font-size: 20px; margin-bottom: 15px;",
                                        tags$ul(
                                          tags$li("Access live updates on parking lot availability, reducing the stress of finding a spot.", style = "font-size: 16px;"),
                                          tags$li("Plan your trips confidently with accurate predictions based on past data.", style = "font-size: 16px;")  # Adjust the font size as needed
                                        )
                                ),
                                tags$li("2. Comprehensive Location Information:",
                                        style = "font-weight: bold; font-size: 20px; margin-bottom: 15px;",
                                        tags$ul(
                                          tags$li("Discover nearby hawker centers, allowing you to enjoy Singapore's diverse culinary scene effortlessly.", style = "font-size: 16px;"),
                                          tags$li("Find parks for leisure and relaxation, enhancing your travel experience beyond just parking solutions.", style = "font-size: 16px;")
                                        )
                                ),
                                tags$li("3. Transparent Parking Rates:",
                                        style = "font-weight: bold; font-size: 20px; margin-bottom: 15px;",
                                        tags$ul(
                                          tags$li("Access detailed information on parking rates for better financial planning.", style = "font-size: 16px;"),
                                          tags$li("Make informed decisions with upfront knowledge of costs, ensuring no surprises when it comes to parking fees.", style = "font-size: 16px;")
                                        )
                                )
                              )
                            )
                          )
                 ), 
                 navbarMenu("Lots Availability",
                            tabPanel("Malls",
                                     fluidPage(
                                       titlePanel("Mall Parking Lots Availability"),
                                       # Add a selector input for the location
                                       selectInput("mall_location_selector", "Select Location:",
                                                   choices = mall_carpark_name),
                                       # Add a selector input for days of the week
                                       selectInput("mall_day_selector", "Select Day:",
                                                   choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                                       # Add a "Go!" button
                                       actionButton("mall_go_button", "Go!"),
                                       # Placeholder for the carpark bar chart output
                                       plotOutput("mall_carpark_chart")
                                     ),
                                     # Text to display below the plot
                                     tags$p("Values in ", 
                                            tags$span(style = "color: red; font-weight: bold;", "red"), 
                                            " are those with estimated max capacity of 25% and below.")
                            ),
                            tabPanel("HDB",
                                     fluidPage(
                                       titlePanel("HDB Parking Lots Availability"),
                                       # Add a selector input for the location
                                       selectInput("hdb_location_selector", "Select Location:",
                                                   choices = hdb_carpark_name),
                                       
                                       # Add a "Go!" button
                                       actionButton("hdb_go_button", "Go!"),
                                       
                                       # Placeholder for the carpark bar chart output
                                       plotOutput("hdb_carpark_chart"),
                                       
                                       # Text to display below the plot
                                       tags$p("Values in ", 
                                              tags$span(style = "color: red; font-weight: bold;", "red"), 
                                              " are those with estimated max capacity of 25% and below. 
                                              Some graphs have two charts due to 2 carparks for the same location.")
                                     )
                            )
                 ),
                 # 3rd navigation tab
                 tabPanel("Go Where?",
                          # Include the sidebar and main panel from the first code segment here
                          titlePanel("Map Application"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("location_selector2", "Select Location:",
                                          choices = combined_names),
                              
                              # Start time inputs
                              dateInput("startDate", "Start Date", value = Sys.Date()),
                              div(
                                style = "display: inline-block; vertical-align: top; width: 100px;",
                                selectInput("startHour", "Hour", choices = sprintf("%02d", 0:23))
                              ),
                              div(
                                style = "display: inline-block; vertical-align: top; width: 100px;",
                                selectInput("startMinute", "Minute", choices = sprintf("%02d", 0:59))
                              ),
                              
                              # End time inputs
                              dateInput("endDate", "End Date", value = Sys.Date()),
                              div(
                                style = "display: inline-block; vertical-align: top; width: 100px;",
                                selectInput("endHour", "Hour", choices = sprintf("%02d", 0:23))
                              ),
                              div(
                                style = "display: inline-block; vertical-align: top; width: 100px;",
                                selectInput("endMinute", "Minute", choices = sprintf("%02d", 0:59))
                              ),
                              
                              checkboxGroupInput("features", "Additional Features", 
                                                 choices = c("Hawker", "Mall", "Park")),
                              actionButton("goButton", "Go!")
                            ),
                            mainPanel(
                              leafletOutput("map"),
                              tags$div(
                                style = "margin-top: 20px; background-color: white; padding: 10px;",
                                textOutput("totalCostOutput")
                              )
                            )
                          )
                 )
)

# Server
server <- function(input, output, session) {
  # Reactive function for mall data based on user input
  mall_carpark_data_reactive <- reactive({
    req(input$mall_location_selector, input$mall_day_selector)  # Ensure both inputs are available
    mall_carpark_data <- mall_carpark_data[mall_carpark_data$Carpark == input$mall_location_selector, ]
    day_data <- day_of_the_week(mall_carpark_data, tolower(substr(input$mall_day_selector, 1, 3)))
    return(day_data)
  })
  
  # Reactive expression to capture the Go button click for Mall
  mall_go_button_click <- reactive({
    input$mall_go_button
  })
  
  # Render the mall carpark bar chart
  output$mall_carpark_chart <- renderPlot({
    req(mall_carpark_data_reactive(), mall_go_button_click())  # Ensure reactive data is available
    mall_name <- input$mall_location_selector
    plot_mall_carpark_bar_chart(mall_carpark_data_reactive(), mall_name)
  })
  
  # Placeholder reactive data for the HDB carpark data 
  hdb_carpark_data_reactive <- reactive({
    req(input$hdb_location_selector, input$hdb_go_button)  # Ensure both inputs are available
    hdb_location_data <- hdb_carpark_data[hdb_carpark_data$Carpark == input$hdb_location_selector, ]
    return(hdb_location_data)
  })
  
  # Reactive expression to capture the Go button click for HDB
  hdb_go_button_click <- reactive({
    input$hdb_go_button
  })
  
  # Render the HDB carpark bar chart
  output$hdb_carpark_chart <- renderPlot({
    req(hdb_carpark_data_reactive(), hdb_go_button_click())  # Ensure reactive data and button click are available
    hdb_carpark_name <- input$hdb_location_selector
    plot_hdb_carpark_bar_chart(hdb_carpark_data_reactive(), hdb_carpark_name)
  })
  
  # Function to plot mall carpark bar chart for a specific day
  plot_mall_carpark_bar_chart <- function(mall_carpark_data, mall_carpark_name) {
    # Filter data for the specified carpark
    mall_carpark <- mall_carpark_data[mall_carpark_data$Carpark == mall_carpark_name, ]
    
    # Reshape data for plotting - convert from wide to long format
    mall_carpark_long <- reshape2::melt(mall_carpark_data, id.vars = "Carpark", variable.name = "Hour", value.name = "Lots Available")
    
    # Use gsub to replace "0*" with 0 in the specific column
    mall_carpark_long$`Lots Available` <- as.numeric(gsub("0\\*", "0", as.character(mall_carpark_long$`Lots Available`)))
    
    # Replace NaN with 0
    mall_carpark_long[is.na(mall_carpark_long)] <- 0
    
    # Calculate full carpark capacity using max lots for each carpark
    mall_carpark_long <- mall_carpark_long %>%
      mutate(Full_Cap = max(`Lots Available`),  # Set Full_Cap to the maximum value in Lots Available
             Perc_Available = `Lots Available` / Full_Cap * 100)
    
    # Calculate the percentage of lots available
    mall_carpark_long$Perc_Available <- mall_carpark_long$`Lots Available` / mall_carpark_long$Full_Cap * 100
    
    # Reorder the levels of the "Hour" factor based on their numeric values
    mall_carpark_long$Hour <- factor(mall_carpark_long$Hour, levels = mall_carpark_long$Hour[order(as.numeric(mall_carpark_long$Hour))])
    
    # Vector of day names
    target_day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    # Create a bar chart with customized fill colors - red for lots available <= 25% of full capacity
    ggplot(mall_carpark_long, aes(x = Hour, y = `Lots Available`, fill = Perc_Available <= 25)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_discrete(limits = mall_carpark_long$Hour[order(as.numeric(mall_carpark_long$Hour))]) +  # Set the order of x-axis
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightgreen")) +
      labs(title = paste("Carpark Lots Availability in", mall_carpark_name),
           x = "Hour",
           y = "Lots Available") +
      theme_minimal()
  }
  
  # Function to plot HDB carpark bar chart for a specific carpark
  plot_hdb_carpark_bar_chart <- function(hdb_carpark_data, hdb_carpark_name) {
    # Filter data for the specified carpark
    hdb_carpark <- hdb_carpark_data[hdb_carpark_data$Carpark == hdb_carpark_name, ]
    
    # Reshape data for plotting - convert from wide to long format
    hdb_carpark_long <- reshape2::melt(hdb_carpark, id.vars = "Carpark", variable.name = "Hour", value.name = "Lots Available")
    
    # Get the maximum value for the specified carpark
    max_lots <- hdb_carpark$max[1]
    
    # Calculate the percentage of lots available
    hdb_carpark_long <- hdb_carpark_long %>%
      mutate(Perc_Available = `Lots Available` / max_lots * 100)
    
    # Plot the carpark bar chart
    ggplot(hdb_carpark_long, aes(x = Hour, y = `Lots Available`, fill = Perc_Available <= 25)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightgreen")) +
      labs(title = paste("Carpark Lots Availability -", hdb_carpark_name),
           x = "Hour",
           y = "Lots Available") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  # Example usage:
  # plot_hdb_carpark_bar_chart(data_frame, "A0046")
  
  
  # Define the custom icon for hawker locations
  foodIcon <- makeIcon(
    iconUrl = "hawker.jpeg",  # Replace with the actual path or URL to your food icon image
    iconWidth = 25, iconHeight = 25  # Adjust size as needed
  )
  mallIcon <- makeIcon(
    iconUrl = "mall.jpeg",  # Replace with actual path or URL to mall icon
    iconWidth = 25, iconHeight = 25
  )
  parkIcon <- makeIcon(
    iconUrl = "park.jpeg",  # Replace with actual path or URL to park icon
    iconWidth = 25, iconHeight = 25
  )
  
  carparkIcon <- makeIcon(
    iconUrl = "carpark.jpeg",  # Replace with actual path or URL to park icon
    iconWidth = 25, iconHeight = 25
  )
  
  selected_coords <- reactive({
    req(input$location_selector2)  # Make sure a location has been selected

    
    # Look up the coordinates of the selected location
    selected_location <- combined_df %>% 
      filter(name == input$location_selector2) %>%
      slice(1)  # Take the first row in case there are multiple matches
    
    if(nrow(selected_location) == 1) {
      list(lng = selected_location$long[1], lat = selected_location$lat[1])
    } else {
      NULL  # Return NULL if no match is found
    }
  })
  # Initial map rendering
  output$map <- renderLeaflet({
    coords <- req(selected_coords())
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = coords$lng, lat = coords$lat, zoom = 16) %>%
      addMarkers(data = df_mall_carparks, ~longitude, ~latitude, popup = ~name, icon = carparkIcon, clusterOptions = markerClusterOptions())  %>% 
      addMarkers(data = df_hdblots, ~long, ~lat, popup = ~Result.carparkNo, icon = carparkIcon, clusterOptions = markerClusterOptions())    
  })
  
  
  observeEvent(input$goButton, {
    # Clear existing markers
    #leafletProxy("map") %>% clearMarkers()
    
    # Display hawker locations if "Hawker" is selected
    if ("Hawker" %in% input$features) {
      leafletProxy("map") %>%
        addMarkers(data = df_hawker, ~X, ~Y, popup = ~Name, icon = foodIcon, clusterOptions = markerClusterOptions())
    }
    
    # Display mall locations if "Mall" is selected
    if ("Mall" %in% input$features) {
      leafletProxy("map") %>%
        addMarkers(data = df_mall, ~longitude, ~latitude, popup = ~name, icon = mallIcon, clusterOptions = markerClusterOptions()) 
    }
    
    # Display park locations if "Park" is selected
    if ("Park" %in% input$features) {
      leafletProxy("map") %>%
        addMarkers(data = df_park, ~lon, ~lat, popup = ~details, icon = parkIcon, clusterOptions = markerClusterOptions())
    }
    
    
    times <- reactive({
      req(input$startDate, input$endDate, input$startHour, input$startMinute, input$endHour, input$endMinute)
      start_time <- as.POSIXct(paste(input$startDate, input$startHour, input$startMinute), format="%Y-%m-%d %H %M")
      end_time <- as.POSIXct(paste(input$endDate, input$endHour, input$endMinute), format="%Y-%m-%d %H %M")
      list(start = start_time, end = end_time)
    })
    
    # Reactive expression for the rates data
    rates_data <- reactive({
      req(input$location_selector2)
      
      rates <- NULL
      
      # Try to get rates data from df_hdbprice
      rates <- tryCatch({
        find_carpark_rates(input$location_selector2, df_hdbprice)
        
      }, error = function(e) {
        # If there's an error, try df_mallprice
        tryCatch({
          find_carpark_rates(input$location_selector2, df_mallprice)
        }, error = function(e) {
          # If there's an error in the second attempt, return NULL
          NULL
        })
      })
    })
    
    # Render the total cost output
    output$totalCostOutput <- renderText({
      time_values <- times()
      rate_values <- rates_data()
      
      total_cost_all_days <- calculate_parking_cost(time_values$start, time_values$end, rate_values, rate_values[4][[1]], rate_values[5][[1]])
      
      #if (total_cost_all_days == "No pricing data") {
        #"No pricing data"
        #rate_values <- find_carpark_rates(input$location_selector2, df_mallprice)
        #total_cost_all_days <- calculate_parking_cost(time_values$start, time_values$end, rate_values, rate_values[4][[1]], rate_values[5][[1]])
      #}
      
      if (total_cost_all_days != "No pricing data") {
        paste("Total parking cost for the selected time period is: $", total_cost_all_days)
      } else {
        "Pricing data is not available for the selected carpark and time period."
      }
    })
    
    
  })
  
}


# Run the Shiny app
shinyApp(ui, server)



