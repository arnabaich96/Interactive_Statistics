library(shiny)
library(ggplot2)
library(DT)


ui <- fluidPage(
  # Data input type selection
  radioButtons("data_input_type", "Data Input Type", choices = c("Import File", "Manual Entry")),
  # File input for import option
  conditionalPanel(
    condition = "input$data_input_type == 'Import File'",
    fileInput("file", "Upload CSV or TXT File")
  ),
  # Manual entry fields for manual option
  conditionalPanel(
    condition = "input$data_input_type == 'Manual Entry'",
    textInput("row_name", "Row Name"),
    textInput("col_name", "Column Name"),
    dataTableOutput("data_table")
  ),
  # Output
  tableOutput("result_table"),
  plotOutput("chisq_plot")
)
server <- function(input, output) {
  # Reactive value for data
  data <- reactiveValues()

  # Observe file input and update data
  observeEvent(input$file, {
    data$df <- read.csv(input$file$datapath)
  })

  # Observe manual entry and update data
  observeEvent(input$row_name, {
    if (!is.null(input$row_name) && !is.null(input$col_name)) {
      data$df <- data.frame(
        Row = rep(input$row_name, 2),
        Column = c(input$col_name, input$col_name),
        Value = numeric(2)
      )
    }
  })

  # Interactive data table
  output$data_table <- renderDataTable({
    datatable(data$df, editable = TRUE)
  })

  # Update data frame when table is edited
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    data$df[info$row, info$col] <- info$value
  })

  # Chi-square test and results
  chisq_result <- reactive({
    # Create contingency table from data
    table <- table(data$df[, 2], data$df[, 3])
    chisq.test(table)
  })

  # Output results
  output$result_table <- renderTable({
    # ... (same as before)
  })

  output$chisq_plot <- renderPlot({
    # ... (same as before)
  })
}
shinyApp(ui, server)
