ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      width = 4,
      radioButtons("data_input_type", "Choose Data Input Method:",
                   choices = c("Upload File", "Manual Entry", "Prebuilt Dataset")),

      # File upload option
      conditionalPanel(
        condition = "input.data_input_type == 'Upload File'",
        fileInput("file", "Upload CSV or TXT File")
      ),

      # Manual entry option
      conditionalPanel(
        condition = "input.data_input_type == 'Manual Entry'",
        numericInput("rows", "Number of Rows:", 3, min = 1),
        numericInput("cols", "Number of Columns:", 3, min = 1),
        actionButton("generate_table", "Generate Table")
      ),

      # Prebuilt dataset option
      conditionalPanel(
        condition = "input.data_input_type == 'Prebuilt Dataset'",
        selectInput("prebuilt_dataset", "Select Dataset", choices = c("HairEyeColor", "UCBAdmissions"))
      ),

      # Significance level input (appears only if any input method is selected)
      conditionalPanel(
        condition = "input.data_input_type != '' && (input.file != null || input.generate_table > 0 || input.prebuilt_dataset != '')",
        hr(),
        numericInput("alpha", "Significance Level:", 0.05, min = 0, max = 1, step = 0.01),
        actionButton("run_analysis", "Run Analysis")
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",

        # Data Input Tab (only appears if data has been generated or uploaded)
        tabPanel("Data Input",
                 conditionalPanel(
                   condition = "input.generate_table > 0 || input.file != null || input.prebuilt_dataset != ''",
                   DTOutput("data_table")
                 )
        ),

        # Significance Level and Results Tab (only appears after analysis is run)
        tabPanel("Analysis Results",
                 conditionalPanel(
                   condition = "input.run_analysis > 0",
                   gt_output("result_table_gt"),
                   plotlyOutput("chisq_plot")
                 )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive value for data
  data <- reactiveValues(df = NULL)

  # Generate table for manual entry
  observeEvent(input$generate_table, {
    data$df <- as.data.frame(matrix(NA, nrow = input$rows, ncol = input$cols))
    colnames(data$df) <- paste("Col", 1:input$cols, sep = "")
    rownames(data$df) <- paste("Row", 1:input$rows, sep = "")
  })

  # Handle file upload
  observeEvent(input$file, {
    data$df <- read.csv(input$file$datapath)
  })

  # Load prebuilt dataset
  observeEvent(input$prebuilt_dataset, {
    if (input$prebuilt_dataset == "HairEyeColor") {
      data$df <- as.data.frame(apply(HairEyeColor, c(1, 2), sum)) # Summarize the 3rd dimension
    } else if (input$prebuilt_dataset == "UCBAdmissions") {
      data$df <- as.data.frame(apply(UCBAdmissions, c(1, 2), sum)) # Summarize the 3rd dimension
    }
  })

  # Render data table for manual entry or prebuilt datasets
  output$data_table <- renderDT({
    req(data$df)
    datatable(data$df, editable = TRUE)
  })

  # Update data when edited manually
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    data$df[info$row, info$col] <- as.numeric(info$value)
  })

  # Perform chi-square test
  chisq_result <- reactive({
    req(data$df)

    # Assuming data$df is in a correct contingency table format
    table <- as.table(as.matrix(data$df))

    chisq.test(table)
  })

  # Output results after analysis is run
  observeEvent(input$run_analysis, {
    output$result_table_gt <- render_gt({
      result <- chisq_result()
      if (is.null(result)) return(NULL)

      df <- data.frame(
        Term = c("Null Hypothesis", "Alternative Hypothesis", "Test Statistic", "Degrees of Freedom", "P-value", "Conclusion"),
        Value = c(
          "Variables are independent",
          "Variables are not independent",
          round(result$statistic, 4),
          result$parameter,
          round(result$p.value, 4),
          ifelse(result$p.value < input$alpha, "Reject the Null Hypothesis", "Fail to Reject the Null Hypothesis")
        )
      )

      df %>%
        gt() %>%
        tab_header(
          title = "Chi-Square Test Results"
        ) %>%
        cols_label(
          Term = "Term",
          Value = "Value"
        )
    })

    output$chisq_plot <- renderPlotly({
      result <- chisq_result()
      if (is.null(result)) return(NULL)

      df <- result$parameter
      crit_val <- qchisq(1 - input$alpha, df)
      x_max <- max(crit_val, result$statistic, df + 10)

      # Generate x and y values for plotting
      x_vals <- seq(0, x_max * 1.1, length.out = 1000)
      y_vals <- dchisq(x_vals, df = df)
      max_density <- max(y_vals) # Find the maximum density value for scaling the lines

      plot <- plot_ly(x = x_vals, y = y_vals, type = 'scatter', mode = 'lines',
                      line = list(color = 'blue', width = 2),
                      name = 'Chi-Square Distribution') %>%

        # Adding the test statistic line
        add_lines(x = c(result$statistic, result$statistic), y = c(0, max_density),
                  line = list(color = 'red', width = 4),
                  name = 'Test Statistic') %>%

        # Adding the critical value line
        add_lines(x = c(crit_val, crit_val), y = c(0, max_density),
                  line = list(color = 'green', dash = 'dash', width = 4),
                  name = 'Critical Value') %>%

        # Shading the area beyond the test statistic (right-tail)
        add_polygons(x = c(result$statistic, x_vals[x_vals >= result$statistic], x_max * 1.1),
                     y = c(0, y_vals[x_vals >= result$statistic], 0),
                     fillcolor = 'rgba(255, 165, 0, 0.5)', line = list(width = 0),
                     name = 'Shaded P-Value Region') %>%

        # Adding annotations for clarity
        layout(title = 'Chi-Square Distribution with Test Statistic and Critical Value',
               xaxis = list(title = 'Chi-Square Value', range = c(0, x_max * 1.1)), # Extended x-axis range
               yaxis = list(title = 'Density'),
               showlegend = TRUE,
               legend = list(orientation = 'h', # Horizontal legend
                             x = 0.5, # Centered horizontally
                             y = -0.2, # Positioned below the plot
                             xanchor = 'center', # Anchored at the center
                             yanchor = 'top')) # Anchored at the top

      plot
    })
  })
}

list(ui = ui, server = server)
