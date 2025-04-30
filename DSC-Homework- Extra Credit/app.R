
library(shiny)
library(ggplot2)

# Load the dataset
intel_data <- read.csv("Intel-1998.csv", header = TRUE)
# Convert Date column to Date format (month/day/year)
intel_data$Date <- as.Date(intel_data$Date, format = "%m/%d/%Y")

# Define UI
ui <- fluidPage(
  titlePanel("Closing Price vs. Date for Intel Corporation (1998)"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = "1998-01-01", end = "1998-12-31",
                     min = "1998-01-01", max = "1998-12-31"),
      checkboxInput("showTrend", "Show Trend Line", value = TRUE)
    ),
    mainPanel(
      plotOutput("closingPricePlot"),
      tableOutput("debugTable"),  # Debug table for filtered data
      textOutput("summaryText")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive data filtering
  filtered_data <- reactive({
    data <- intel_data[intel_data$Date >= input$dateRange[1] & intel_data$Date <= input$dateRange[2], ]
    data <- data[!is.na(data$Close) & !is.na(data$Date), ]  # Remove rows with missing values
    return(data)
  })
  
  # Render debug table with Date column preserved
  output$debugTable <- renderTable({
    data <- filtered_data()
    data$Date <- format(data$Date, "%m/%d/%Y")  # Ensure dates remain formatted as month/day/year
    data  # Return formatted dataset
  })
  
  # Render line graph
  output$closingPricePlot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) {
      plot.new()
      title(main = "No data available for the selected range")
      return()
    }
    ggplot(data, aes(x = Date, y = Close)) +
      geom_line(color = "blue") +
      labs(title = "Closing Price vs. Date for Intel Corporation (1998)",
           x = "Date", y = "Closing Price") +
      theme_minimal() +
      if (input$showTrend) geom_smooth(method = "loess", color = "red", se = FALSE)
  })
  
  # Render summary text
  output$summaryText <- renderText({
    data <- filtered_data()
    paste("Displaying data for", nrow(data), "days between", 
          format(input$dateRange[1], "%m/%d/%Y"), "and", 
          format(input$dateRange[2], "%m/%d/%Y"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
