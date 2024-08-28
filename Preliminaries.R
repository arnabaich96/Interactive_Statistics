# Load the required libraries
library(shiny)
library(knitr)


# Define the user interface
ui <- fluidPage(
  titlePanel("Sample Mean vs. True Mean"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("sampleSize",
                  "Select Sample Size:",
                  min = 1,
                  max = 200,
                  value = 10)
    ),

    mainPanel(
      h3("Sampled Values"),
      tableOutput("sampledValues"),
      h3("Sample Mean"),
      textOutput("sampleMean"),
      h3("True Mean vs. Sample Mean"),
      plotOutput("meanPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Simulate the population
  population <- rnorm(10^7, mean = 70, sd = 10) # Mean of 70 and SD of 10

  # Reactive expression to get the sample data
  sample_data <- reactive({
    sample(population, input$sampleSize)
  })

  # Display sampled values in a row format
  output$sampledValues <- renderTable({
    t(as.data.frame(sample_data()))
  }, rownames = FALSE, colnames = FALSE)

  # Calculate and display the sample mean
  output$sampleMean <- renderText({
    sample_mean <- mean(sample_data())
    true_mean <- mean(population)
    knitr::kable(data.frame("Sample Mean" = sample_mean, "True Mean" = true_mean))
  })

  # Plot the true mean vs sample mean
  output$meanPlot <- renderPlot({
    true_mean <- mean(population)
    sample_mean <- mean(sample_data())

    hist(population, breaks = 100, main = "Distribution of Population",
         xlab = "Weight", xlim = c(min(population), max(population)), col = "lightblue")
    abline(v = true_mean, col = "blue", lwd = 4)  # Thicker line for true mean
    abline(v = sample_mean, col = "red", lwd = 4)  # Thicker line for sample mean
    legend("topright", legend = c("True Mean", "Sample Mean"),
           col = c("blue", "red"), lwd = 4)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
