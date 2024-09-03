
# Define UI
ui <- fluidPage(
  titlePanel(h1("ANOVA Analysis", align = "center")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      radioButtons("data_input_type", "Choose Data Input Method:",
                   choices = c("Upload File", "Prebuilt Dataset")),

      # File upload option
      conditionalPanel(
        condition = "input$data_input_type == 'Upload File'",
        fileInput("file", "Upload CSV or TXT File")
      ),

      # Prebuilt dataset option
      conditionalPanel(
        condition = "input$data_input_type == 'Prebuilt Dataset'",
        selectInput("prebuilt_dataset", "Select Dataset",
                    choices = c("iris (One-Way ANOVA)" = "iris",
                                "ToothGrowth (Two-Way ANOVA)" = "ToothGrowth"))
      ),

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

        # ANOVA Results Tab
        tabPanel("ANOVA Results",
                 uiOutput("anova_table"),
                 uiOutput("mean_effects")
        )
      )
    )
  )
)

# Define Server logic
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
                 "iris" = iris[, c("Sepal.Length", "Species")],
                 "ToothGrowth" = ToothGrowth)
    data(df)
  })

  # Display the data in the Data Input tab
  output$data_table <- renderDT({
    req(data())
    datatable(data())
  })

  # Perform ANOVA analysis when the button is clicked
  observeEvent(input$run_analysis, {
    df <- data()
    req(df)

    anova_model <- NULL

    # Determine the type of ANOVA based on the number of columns
    if (ncol(df) == 2) {
      # One-Way ANOVA
      colnames(df) <- c("Response", "Factor")
      df$Factor <- as.factor(df$Factor)
      anova_model <- aov(Response ~ Factor, data = df)
    } else if (ncol(df) == 3) {
      # Two-Way ANOVA
      colnames(df) <- c("Response", "Factor1", "Factor2")
      df$Factor1 <- as.factor(df$Factor1)
      df$Factor2 <- as.factor(df$Factor2)
      anova_model <- aov(Response ~ Factor1 * Factor2, data = df)
    }

    # Display ANOVA table
    output$anova_table <- renderUI({
      req(anova_model)
      anova_result <- summary(anova_model)

      # Create a data frame from the ANOVA summary
      anova_table <- as.data.frame(anova_result[[1]])
      anova_table$Source <- rownames(anova_table)  # Add row names as a column

      # Use the gt package to create the table
      anova_table_gt <- anova_table %>%
        gt() %>%
        tab_header(title = "ANOVA Table") %>%
        cols_label(
          Source = "Source",
          Df = "Degrees of Freedom",
          `Sum Sq` = "Sum of Squares",
          `Mean Sq` = "Mean Squares",
          `F value` = "F Value",
          `Pr(>F)` = "P Value"
        )

      anova_table_gt
    })

    # Display mean effects
    output$mean_effects <- renderUI({
      req(anova_model)

      if (ncol(df) == 2) {
        # One-Way ANOVA mean effects
        factor_name <- colnames(df)[2]
        means <- tapply(df$Response, df$Factor, mean)
        mean_effects_df <- data.frame(
          Factor = names(means),
          Mean = round(means, 2)
        )

        mean_effects_gt <- mean_effects_df %>%
          gt() %>%
          tab_header(title = paste("Mean Effects for", factor_name)) %>%
          cols_label(
            Factor = factor_name,
            Mean = "Mean Response"
          )

        mean_effects_gt

      } else if (ncol(df) == 3) {
        # Two-Way ANOVA mean effects
        factor_name1 <- colnames(df)[2]
        factor_name2 <- colnames(df)[3]
        means_factor1 <- tapply(df$Response, df$Factor1, mean)
        means_factor2 <- tapply(df$Response, df$Factor2, mean)
        mean_effects_df1 <- data.frame(
          Factor = names(means_factor1),
          Mean = round(means_factor1, 2)
        )
        mean_effects_df2 <- data.frame(
          Factor = names(means_factor2),
          Mean = round(means_factor2, 2)
        )

        mean_effects_gt1 <- mean_effects_df1 %>%
          gt() %>%
          tab_header(title = paste("Mean Effects for", factor_name1)) %>%
          cols_label(
            Factor = factor_name1,
            Mean = paste("Mean", factor_name1)
          )

        mean_effects_gt2 <- mean_effects_df2 %>%
          gt() %>%
          tab_header(title = paste("Mean Effects for", factor_name2)) %>%
          cols_label(
            Factor = factor_name2,
            Mean = paste("Mean", factor_name2)
          )

        # Render each table separately
        tagList(
          mean_effects_gt1 %>% as_raw_html() %>% HTML(),
          mean_effects_gt2 %>% as_raw_html() %>% HTML()
        )
      }
    })
  })
}

list(ui = ui, server = server)

