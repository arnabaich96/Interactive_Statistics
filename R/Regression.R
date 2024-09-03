
# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(h1("Regression Analysis", align = "center")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      radioButtons("data_input_type", "Choose Data Input Method:",
                   choices = c("Upload File", "Prebuilt Dataset")),

      # File upload option
      conditionalPanel(
        condition = "input$data_input_type != 'Prebuilt Dataset'",
        fileInput("file", "Upload CSV or TXT File")
      ),

      # Prebuilt dataset option
      conditionalPanel(
        condition = "input$data_input_type != 'Upload File'",
        selectInput("prebuilt_dataset", "Select Dataset",
                    choices = c("mtcars (Multiple Regression)",
                                "faithful (Simple Regression)"))
      ),

      hr(),
      checkboxInput("include_intercept", "Include Intercept", value = TRUE),

      hr(),
      uiOutput("x_variable_selector"),

      hr(),
      actionButton("run_analysis", "Run Analysis")
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",

        # Data Input Tab
        tabPanel("Data Input",
                 DTOutput("data_table")
        ),

        # Scatter Plot Tab
        tabPanel("Diagrams",
                 conditionalPanel(
                   condition = "output.showScatterPlot == TRUE",
                   plotlyOutput("scatter_plot"),
                   plotlyOutput("residual_plot")
                 )
        ),

        # Regression Summary Tab
        tabPanel("Regression Summary",
                 uiOutput("summary_table")
        ),

        # ANOVA Table Tab
        tabPanel("ANOVA Table",
                 uiOutput("anova_table")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive value for data
  data <- reactiveVal()

  # Observe file input and update data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    data(df)
  })

  # Observe prebuilt dataset selection and update data
  observeEvent(input$prebuilt_dataset, {
    req(input$prebuilt_dataset)
    df <- switch(input$prebuilt_dataset,
                 "mtcars (Multiple Regression)" = mtcars,
                 "faithful (Simple Regression)" = faithful)
    data(df)
  })

  # Display the data in the Data Input tab
  output$data_table <- renderDT({
    req(data())
    datatable(data())
  })

  # Create dynamic input for selecting x-variable for scatter plot
  output$x_variable_selector <- renderUI({
    df <- data()
    if (is.null(df) || ncol(df) <= 2) return(NULL)
    selectInput("x_variable", "Select X variable for Scatter Plot:", choices = names(df)[-1])
  })

  # Reactive expression to determine if scatter plot should be shown
  output$showScatterPlot <- reactive({
    ncol(data()) == 2 || !is.null(input$x_variable)
  })
  outputOptions(output, "showScatterPlot", suspendWhenHidden = FALSE)

  # Perform regression analysis when the button is clicked
  observeEvent(input$run_analysis, {
    df <- data()
    req(df)

    # Formulate the regression formula
    formula <- if (input$include_intercept) {
      as.formula(paste0(names(df)[1], " ~ ."))
    } else {
      as.formula(paste0(names(df)[1], " ~ . - 1"))
    }

    # Fit the regression model
    model <- lm(formula, data = df)

    # Plot the regression using Plotly or ggplot2 depending on the number of predictors
    output$scatter_plot <- renderPlotly({
      req(df)
      if (ncol(df) == 2) {
        # Simple Linear Regression Plot
        ggplotly(ggplot(df, aes_string(x = names(df)[2], y = names(df)[1])) +
                   geom_point() + theme_classic() +
                   geom_smooth(method = "lm", se = FALSE) +
                   labs(title = "Scatter Plot with Regression Line",
                        x = names(df)[2],
                        y = names(df)[1])
        )
      } else if (!is.null(input$x_variable)) {
        # Scatter Plot with a selected X variable for Multiple Linear Regression
        ggplotly(ggplot(df, aes_string(x = input$x_variable, y = names(df)[1])) +
                   geom_point() + theme_classic() +
                   geom_smooth(method = "lm", se = FALSE) +
                   labs(title = paste("Scatter Plot of", names(df)[1], "vs", input$x_variable),
                        x = input$x_variable,
                        y = names(df)[1])
        )
      }
    })

    output$residual_plot <- renderPlotly({
      # Residual Plot for the overall model
      ggplotly(ggplot(model, aes_string(x = ".fitted", y = ".resid")) +
                 geom_point() + theme_classic() +
                 geom_hline(yintercept = 0, color = "red") +
                 labs(title = "Residual Plot",
                      x = "Fitted Values",
                      y = "Residuals")
      )
    })

    # Display the ANOVA table
    output$anova_table <- renderUI({
      anova_df <- anova(model)
      SST <- sum(anova_df$`Sum Sq`)
      SSR <- anova_df$`Sum Sq`[1]
      SSE <- anova_df$`Sum Sq`[2]
      df_total <- sum(anova_df$Df)
      df_regression <- anova_df$Df[1]
      df_error <- anova_df$Df[2]
      MSR <- SSR / df_regression
      MSE <- SSE / df_error
      F_value <- MSR / MSE
      p_value <- pf(F_value, df_regression, df_error, lower.tail = FALSE)

      anova_html <- paste0(
        "<table border='1' cellpadding='5' cellspacing='0' width='100%' style='text-align:center;'>",
        "<tr><th style='font-weight:bold;'>Source</th>",
        "<th style='font-weight:bold;'>SS</th>",
        "<th style='font-weight:bold;'>df</th>",
        "<th style='font-weight:bold;'>MS</th>",
        "<th style='font-weight:bold;'>F</th>",
        "<th style='font-weight:bold;'>Pr(>F)</th></tr>",
        "<tr><td>Regression</td>",
        "<td>", round(SSR, 3), "</td>",
        "<td>", df_regression, "</td>",
        "<td>", round(MSR, 3), "</td>",
        "<td>", round(F_value, 3), "</td>",
        "<td>", round(p_value, 3), "</td></tr>",
        "<tr><td>Error</td>",
        "<td>", round(SSE, 3), "</td>",
        "<td>", df_error, "</td>",
        "<td>", round(MSE, 3), "</td>",
        "<td></td>",
        "<td></td></tr>",
        "<tr><td>Total</td>",
        "<td>", round(SST, 3), "</td>",
        "<td>", df_total, "</td>",
        "<td></td>",
        "<td></td>",
        "<td></td></tr></table>"
      )

      HTML(anova_html)
    })

    # Conditionally display the summary outputs based on user selection
    output$summary_table <- renderUI({
      coef_summary <- summary(model)$coefficients

      summary_html <- paste0(
        "<table border='1' cellpadding='5' cellspacing='0' width='100%' style='text-align:center;'>",
        "<tr><th style='font-weight:bold;'>Term</th>",
        "<th style='font-weight:bold;'>Estimate</th>",
        "<th style='font-weight:bold;'>Std. Error</th>",
        "<th style='font-weight:bold;'>t value</th>",
        "<th style='font-weight:bold;'>Pr(>|t|)</th></tr>"
      )

      for (i in 1:nrow(coef_summary)) {
        summary_html <- paste0(
          summary_html,
          "<tr><td>", rownames(coef_summary)[i], "</td>",
          "<td>", round(coef_summary[i, "Estimate"], 3), "</td>",
          "<td>", round(coef_summary[i, "Std. Error"], 3), "</td>",
          "<td>", round(coef_summary[i, "t value"], 3), "</td>",
          "<td>", round(coef_summary[i, "Pr(>|t|)"], 3), "</td></tr>"
        )
      }

      summary_html <- paste0(summary_html, "</table>")
      HTML(summary_html)
    })
  })
}

# Run the application
list(ui = ui, server = server)
