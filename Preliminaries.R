---
  title: "Sample Mean vs. True Mean"
output: html_document
---

  ## Introduction

  Sampling is the process of selecting a subset of individuals from a population to estimate characteristics of the whole population. The accuracy of statistical estimates, like the sample mean, is affected by the sample size. Larger samples tend to provide more reliable estimates because they better represent the population. However, even small samples can give valuable insights, though with greater variability. By exploring different sample sizes in the app below, you can observe how the sample mean fluctuates and compare it to the true mean of the population.

## Shiny App

```{r, echo=FALSE}
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
      h3("Sample Mean vs. True Mean"),
      tableOutput("meanTable"),
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

  # Display sampled values in a 6x5 table format
  output$sampledValues <- renderTable({
    sample_values <- sample_data()
    if (length(sample_values) > 30) {
      sample_values <- sample_values[1:30]
    }
    matrix(sample_values, nrow = 6, byrow = TRUE)
  }, rownames = TRUE, colnames = FALSE)

  # Display the sample mean and true mean in a table format
  output$meanTable <- renderTable({
    sample_mean <- mean(sample_data())
    true_mean <- mean(population)
    data.frame("Sample Mean" = round(sample_mean, 2), "True Mean" = round(true_mean, 2))
  }, rownames = FALSE)

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

# Run the application within the document
shinyApp(ui = ui, server = server)
