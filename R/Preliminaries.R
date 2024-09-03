
# Define the user interface
ui <- fluidPage(
  titlePanel(h1("Preliminaries", align = "center")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sampleSize",
                  "Select Sample Size:",
                  min = 1,
                  max = 250,
                  value = 10),
      h3("Sample Heights (cm)"),
      tableOutput("sampledValues"),
      h3(" "),
      tableOutput("meanTable")
    ),

    mainPanel(

      h3(" "),
      plotOutput("meanPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Simulate the population (height in cm)
  population <- rnorm(10^7, mean = 170, sd = 10) # Mean of 170 cm and SD of 10 cm

  # Reactive expression to get the sample data
  sample_data <- reactive({
    sample(population, input$sampleSize)
  })

  # Display sampled values in a 10x2 table format
  output$sampledValues <- renderTable({
    sample_values <- sample_data()
    if (length(sample_values) > 20) {
      sample_values <- sample_values[1:20]
    }
    matrix(sample_values, ncol = 3,nrow = 3, byrow = TRUE)
  }, rownames = TRUE, colnames = FALSE)

  # Display the sample mean and true mean in a table format
  output$meanTable <- renderTable({
    sample_mean <- mean(sample_data())
    true_mean <- mean(population)
    cbind(c("Sample_size" ,"Sample_Mean" ,"Population_Mean"), c(input$sampleSize,round(sample_mean, 2),  round(true_mean, 2)))
  }, colnames  = FALSE)

  # Plot the true mean vs sample mean
  output$meanPlot <- renderPlot({
    true_mean <- mean(population)
    sample_mean <- mean(sample_data())

    hist(population, breaks = 100, main = "Height Distribution in Population",
         xlab = "Height (cm)", xlim = c(min(population), max(population)), col = "lightblue")
    abline(v = true_mean, col = "blue", lwd = 4)  # Thicker line for population mean
    abline(v = sample_mean, col = "red", lwd = 4)  # Thicker line for sample mean
    legend("topright", legend = c("Population Mean", "Sample Mean"),
           col = c("blue", "red"), lwd = 4)
  })
}

list(ui = ui, server = server)
