
# UI ----------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      class = "box",
      width = 5,  # Adjust width to fit inputs
      selectInput("data_entry_type", "Select Data Entry Type:",
                  choices = c("Generate Data", "File Upload", "Manual Input"),
                  selected = "Generate Data"),

      # Conditional panel for file upload
      conditionalPanel(
        condition = "input.data_entry_type == 'File Upload'",
        fileInput("file", "Upload a CSV or .txt File with One Row of Values", accept = c(".csv", ".txt"))
      ),

      # Conditional panel for manual data entry
      conditionalPanel(
        condition = "input.data_entry_type == 'Manual Input'",
        textInput("data", "Enter Numbers Separated by Commas:", "")),
      conditionalPanel(
        condition = "input.data_entry_type == 'Generate Data'",
        numericInput("sample_size", "Sample Size:", value = 100, min = 1),
        selectInput("dist_type", "Select Distribution:",
                    choices = c("Normal", "Gamma", "Inverse Gaussian", "t-distribution",
                                "Chi-squared", "F-distribution", "Binomial", "Uniform")),
        fluidRow(
          column(6,
                 conditionalPanel(
                   condition = "input.dist_type == 'Normal'",
                   numericInput("normal_mean", "Mean:", value = 0)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Gamma'",
                   numericInput("gamma_shape", "Shape:", value = 2)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Inverse Gaussian'",
                   numericInput("ig_mean", "Mean:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 't-distribution'",
                   numericInput("t_df", "Degrees of Freedom:", value = 10, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Chi-squared'",
                   numericInput("chisq_df", "Degrees of Freedom:", value = 10, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'F-distribution'",
                   numericInput("f_df1", "Degrees of Freedom 1:", value = 10, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Binomial'",
                   numericInput("binom_size", "Number of Trials:", value = 10, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Uniform'",
                   numericInput("uniform_min", "Minimum:", value = 0)
                 )
          )
      )
      ,

          column(6,
                 conditionalPanel(
                   condition = "input.dist_type == 'Normal'",
                   numericInput("normal_sd", "Standard Deviation:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Gamma'",
                   numericInput("gamma_rate", "Rate:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Inverse Gaussian'",
                   numericInput("ig_dispersion", "Dispersion (lambda):", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'F-distribution'",
                   numericInput("f_df2", "Degrees of Freedom 2:", value = 10, min = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Binomial'",
                   numericInput("binom_prob", "Probability of Success:", value = 0.5, min = 0, max = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type == 'Uniform'",
                   numericInput("uniform_max", "Maximum:", value = 1)
                 )
          )
        ),
      fluidRow(
        column(6, numericInput("quantile_p", "Quantile (p):", value = 0.5, min = 0, max = 1)),
        column(6, numericInput("bins", "Number of bins for histogram:", value = 10, min = 1))
      ),

      fluidRow(
        column(6, numericInput("value_to_zscore", "Value to Calculate Z-Score:", value = 0)),
        column(6, numericInput("zscore_to_prob", "Z-Score of value:", value = 1))
      ),

      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      width = 7,  # Adjust width to fit plots and table
      fluidRow(
        column(6, div(class = "header-box", h3("Boxplot")), div(class = "content-box", plotlyOutput("boxplot"))),
        column(6, div(class = "header-box", h3("Histogram with Density Curve")), div(class = "content-box", plotlyOutput("histogram")))
      ),
      fluidRow(
        div(class = "header-box", h3("Summary Statistics")),
        div(class = "content-box", gt_output("summaryStats"))
      )
    )
  )
)
# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observe({
    if(input$analyze == 0) return(NULL)

    data <- reactive({
      if (input$data_entry_type == "File Upload" && !is.null(input$file)) {
        # Read data from uploaded file (either .csv or .txt)
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
          file_data <- read.csv(input$file$datapath, header = FALSE)
        } else if (ext == "txt") {
          file_data <- read.table(input$file$datapath, sep = ",", header = FALSE)
        }
        as.numeric(file_data[1, ])

      } else if (input$data_entry_type == "Manual Input" && input$data != "") {
        # Use manually entered data
        as.numeric(unlist(strsplit(input$data, ",")))

      } else if (input$data_entry_type == "Generate Data") {
        # Generate data based on the selected distribution
        if (input$dist_type == "Normal") {
          rnorm(input$sample_size, mean = input$normal_mean, sd = input$normal_sd)
        } else if (input$dist_type == "Gamma") {
          rgamma(input$sample_size, shape = input$gamma_shape, rate = input$gamma_rate)
        } else if (input$dist_type == "Inverse Gaussian") {
          rinvgauss(input$sample_size, mean = input$ig_mean, shape = input$ig_dispersion)
        } else if (input$dist_type == "t-distribution") {
          rt(input$sample_size, df = input$t_df)
        } else if (input$dist_type == "Chi-squared") {
          rchisq(input$sample_size, df = input$chisq_df)
        } else if (input$dist_type == "F-distribution") {
          rf(input$sample_size, df1 = input$f_df1, df2 = input$f_df2)
        } else if (input$dist_type == "Binomial") {
          rbinom(input$sample_size, size = input$binom_size, prob = input$binom_prob)
        } else if (input$dist_type == "Uniform") {
          runif(input$sample_size, min = input$uniform_min, max = input$uniform_max)
        }
      }
    })

    output$summaryStats <- renderText({
      req(input$analyze)
      isolate({
        d <- data()
        stats <- tibble::tibble(
          Metric = c("Mean", "Median", "SD", "IQR", paste0( input$quantile_p, "th Quantile")),
          Value = round(c(mean(d), median(d), sd(d), IQR(d), quantile(d, probs = input$quantile_p)), 3)
        )

        # Calculate Z-Score if a value is provided
        if (!is.null(input$value_to_zscore)) {
          zscore <- (input$value_to_zscore - mean(d)) / sd(d)
          stats <- add_row(stats, Metric = paste("Z-Score of ", input$value_to_zscore), Value = round(zscore, 3))
        }

        # Calculate Probability from Z-Score if a Z-Score is provided
        if (!is.null(input$zscore_to_prob)) {
          prob <- pnorm(input$zscore_to_prob)
          stats <- add_row(stats, Metric = paste("Value of ", input$zscore_to_prob), Value = round(prob, 3))
        }

        # Transpose the table
        transposed_stats <- t(stats)

        # Convert to knitr::kable and style with kableExtra
        knitr::kable(transposed_stats, format = "html", col.names = NULL) %>%
          kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = T) %>%
          kableExtra::row_spec(1, bold = TRUE, background = "#D3D3D3") %>%  # Highlight the metric row
          kableExtra::column_spec(1, background = "#f2f2f2", bold = TRUE)  # Background for the entire table
      })
    })

    output$boxplot <- renderPlotly({
      req(input$analyze)
      isolate({
        d <- data()
        plot_ly(
          x = ~d,
          type = "box",
          orientation = 'h',
          boxpoints = "outliers",
          marker = list(color = 'blue', size = 4)
        ) %>%
          layout(
            xaxis = list(title = "Value"),
            yaxis = list(showticklabels = FALSE),
            showlegend = FALSE
          )
      })
    })

    output$histogram <- renderPlotly({
      req(input$analyze)
      isolate({
        d <- data()
        hist_data <- hist(d, breaks = input$bins, plot = FALSE)

        # Calculate quartiles
        q1 <- quantile(d, probs = 0.25)
        q3 <- quantile(d, probs = 0.75)

        plot_ly(x = hist_data$mids, y = hist_data$density, type = "bar", name = "Histogram") %>%
          add_lines(x = density(d)$x, y = density(d)$y, name = "Density Curve", line = list(color = 'blue')) %>%
          add_lines(x = c(mean(d), mean(d)), y = c(0, max(hist_data$density)), name = "Mean", line = list(color = 'red', dash = 'dash')) %>%
          add_lines(x = c(median(d), median(d)), y = c(0, max(hist_data$density)), name = "Median", line = list(color = 'green', dash = 'dash')) %>%
          add_lines(x = c(q1, q1), y = c(0, max(hist_data$density)), name = "Q1", line = list(color = 'purple', dash = 'dash')) %>%
          add_lines(x = c(q3, q3), y = c(0, max(hist_data$density)), name = "Q3", line = list(color = 'orange', dash = 'dash')) %>%
          layout(
            xaxis = list(title = " "),
            yaxis = list(title = " "),
            legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2)  # Legend at the bottom
          )
      })
    })


  })

  # Automatically trigger analyze when input changes after the button is clicked once
  observeEvent(input$analyze, {
    session$onFlushed(once = TRUE, function() {
      observeEvent({
        input$normal_mean; input$normal_sd; input$gamma_shape; input$gamma_rate; input$ig_mean;
        input$ig_dispersion; input$t_df; input$chisq_df; input$f_df1; input$f_df2;
        input$binom_size; input$binom_prob; input$uniform_min; input$uniform_max;
        input$quantile_p; input$value_to_zscore; input$zscore_to_prob; input$sample_size
      }, {
        isolate({
          input$analyze
        })
      })
    })
  })
}

list(ui = ui, server = server)
# runApp(list(ui = ui, server = server))
