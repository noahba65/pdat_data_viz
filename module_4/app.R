#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(hflights)
library(glue)


# Read in carrier data base for renaming airline codes
carriers <- read_csv("data/carriers.csv") %>%
  mutate(
    carrier_name = Carrier %>%
      # Remove punctuation (.,)
      str_remove_all("[.,]") %>%
      # Remove the word "Inc" if it exists
      str_replace_all("\\bInc\\b", "") %>%
      # Extract up to the first three words (if fewer words are present, it keeps them)
      str_extract("^\\S+(\\s+\\S+){0,2}"), 
    
    # Replace "International" with an acronym.
    carrier_name = str_replace(carrier_name, "International", "Int.")
  )

flights <- hflights %>%
  
  # Create a date variable with glue and lubridate
  mutate(date = glue("{Year}-{Month}-{DayofMonth }"),
         date = ymd(date)) %>%
  rename(dep_delay = DepDelay,
         Code = UniqueCarrier) %>%
  
  # Join carrier names by "Code"
  left_join(carriers, by = c("Code")) %>%
  
  # Convert all columns to lower case
  rename_with(str_to_lower) %>%
  
  # Select only relevant columns
  select(date, dep_delay, code, carrier_name) 
  

ui <- fluidPage(
  
  # Application title
  titlePanel("Airline Departure Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Select Date"),
      dateRangeInput("date", "Date Range",
                     min = "2011-01-01", max = "2011-12-31",
                     start = "2011-01-01", end = "2011-12-31"
      ),
      h3("Select Time Step"),
      radioButtons("time_step", "Time Step",
                   choices = c("day", "week", "month")
      ),
      h3("Select Airlines to Compare"),
      h5("Select 2 airlines from the list below. To change selection, you must unselect at least one."),
      checkboxGroupInput("carriers", "Carriers",
                         choices = unique(flights$carrier_name))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ts_plot"),
      plotOutput("box_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Observe carrier selections and limit to 2
  observe({
    selected_carriers <- input$carriers
    if (length(selected_carriers) > 2) {
      # Remove the third selection
      updateCheckboxGroupInput(session, "carriers", selected = selected_carriers[1:2])
    }
  })
  
  output$ts_plot <- renderPlot({
    
    # Convert flights to the unit specified in the input and filtered for
    # date inputs
    flights_unit <- flights %>%
      mutate(date = floor_date(date, unit = input$time_step)) %>%
      filter(date >= input$date[1] & date <= input$date[2]) 
      
    
    # Create a flights time series that aggregates mean departure delay by 
    # the specified time unit
    flights_ts <- flights_unit %>%
      group_by(date) %>%
      reframe(mean_dep_delay = mean(dep_delay, na.rm = TRUE))
    
    # Create a flight time series that also groups by the airlines selected in input
    carrier_ts <- flights_unit %>%
      filter(carrier_name %in% input$carriers) %>%
      group_by(date, carrier_name) %>%
      reframe(mean_dep_delay = mean(dep_delay, na.rm = TRUE))
    
    # Plot flight time series
    ggplot(data = flights_ts) +
      geom_point(aes(x = date, y = mean_dep_delay)) +
      
      # Add line plot comparing selected two airlines
      geom_line(data = carrier_ts, aes(x = date, y = mean_dep_delay, color = carrier_name)) +
      labs(x = "Date", y = "Mean Departure Delay (Minutes)", title = "Departure Delays Over Time", color = "Airline") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            plot.title = element_text(face = "bold", size = 30),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            panel.grid = element_blank())
    
  }) # Close renderPlot for ts_plot
  
  output$box_plot <- renderPlot({
    
    # Convert flights to the unit specified in the input and filtered for
    # date inputs
    flights_unit <- flights %>%
      mutate(date = floor_date(date, unit = input$time_step)) %>%
      filter(date >= input$date[1] & date <= input$date[2]) 
      
    

    
    ggplot(flights_unit, aes(x = carrier_name, y = dep_delay)) +
      geom_boxplot(outlier.shape = NA, fill = "purple") +
      coord_cartesian(ylim = c(-20, 40)) +
      labs(x = "Airline", y = "Departure Delay (Minutes)", title = "Departure Delays by Airline") +
      theme(axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.title = element_text(size = 16),
            plot.title = element_text(face = "bold", size = 30),
            panel.grid = element_blank())
    
  }) # Close renderPlot for box_plot
  
}

# Run the application 
shinyApp(ui = ui, server = server)
