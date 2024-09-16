

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("data_entry_type", "Select Data Entry Type:",
                  choices = c("Generate Data", "File Upload", "Manual Input"),
                  selected = "Generate Data"),

      conditionalPanel(
        condition = "input.data_entry_type == 'File Upload'",
        fileInput("file", "Upload a CSV or .txt File with One Row of Values", accept = c(".csv", ".txt"))
      ),

      conditionalPanel(
        condition = "input.data_entry_type == 'Manual Input'",
        textInput("data", "Enter Numbers Separated by Commas:", "")
      ),

      conditionalPanel(
        condition = "input.data_entry_type == 'Generate Data'",
        fluidRow(
          column(6,
                 selectInput("dist_type", "Distribution:",
                             choices = c("Normal", "Gamma", "Inverse Gaussian", "t-distribution",
                                         "Chi-squared", "F-distribution", "Binomial", "Uniform"))),
          column(6, numericInput("sample_size", "Sample Size:", value = 500, min = 1))
        )
      ),
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
        ),
        column(6,
               conditionalPanel(
                 condition = "input.dist_type == 'Normal'",
                 numericInput("normal_sd", "Std Dev:", value = 1)
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
        column(6, checkboxInput("sd_known", "SD Known", value = FALSE)),
        column(6, conditionalPanel(
          condition = "input.sd_known == true",
          numericInput("known_sd", "Enter SD:", value = 1)
        ))
      ),

      sliderInput("confidence_level", "Confidence Level:", min = 50, max = 99.99, value = 95, step = 0.01),

      checkboxInput("perform_hypothesis", "Perform Hypothesis Test?", value = FALSE),
      conditionalPanel(
        condition = "input.perform_hypothesis == true",
        fluidRow(
          column(4, numericInput("mu_0", HTML("&mu;<sub>0</sub>:"), value = 0)),
          column(4, selectInput("alternative", "H1:",
                                choices = c("Not Equal" = "two.sided", "More" = "greater", "Less" = "less"))),
          column(4, numericInput("alpha", HTML("&alpha;:"), value = 0.05, min = 0, max = 1))
        )
      ),

      actionButton("analyze", "Analyze")
    ),

    mainPanel(
      width = 8,
      fluidRow(
        column(12, uiOutput("ciHeader")),
        column(12, div(class = "content-box", gt_output("ciTable")))
      ),
      fluidRow(
        column(12, div(class = "header-box", h3(" "))),
        column(12, div(class = "content-box", plotOutput("ciPlot")))
      ),
      conditionalPanel(
        condition = "input.perform_hypothesis == true",
        fluidRow(
          column(12, div(class = "header-box", h3("Hypothesis Testing Summary"))),
          column(12, div(class = "content-box", uiOutput("hypothesisResult")))
        ),
        fluidRow(
          column(12, div(class = "header-box", h3(" "))),
          column(12, div(class = "content-box", plotOutput("hypothesisPlot")))
        )
      )
    )
  )
)



# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  data <- reactive({
    if (input$data_entry_type == "File Upload" && !is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        file_data <- read.csv(input$file$datapath, header = FALSE)
      } else if (ext == "txt") {
        file_data <- read.table(input$file$datapath, sep = ",", header = FALSE)
      }
      as.numeric(unlist(file_data[1, ]))
    } else if (input$data_entry_type == "Manual Input" && input$data != "") {
      as.numeric(unlist(strsplit(input$data, ",")))
    } else if (input$data_entry_type == "Generate Data") {
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

  output$ciHeader <- renderUI({
    h3(paste(input$confidence_level, "% Confidence Interval"))
  })

  confidence_result <- reactive({
    d <- data()
    if (input$sd_known) {
      ci_result <- z.test(d, sigma.x = input$known_sd, conf.level = input$confidence_level / 100)
    } else {
      ci_result <- t.test(d, conf.level = input$confidence_level / 100)
    }

    tibble::tibble(
      Metric = c("Lower Bound", "Midpoint (Mean)", "Upper Bound", "Interval Length"),
      Value = c(round(ci_result$conf.int[1], 2), round(ci_result$estimate, 2),
                round(ci_result$conf.int[2], 2), round(diff(ci_result$conf.int), 2))
    )
  })

  output$ciTable <- render_gt({
    ci <- confidence_result()
    ci_transposed <- as.data.frame(t(ci))
    colnames(ci_transposed) <- ci_transposed[1, ]
    ci_transposed <- ci_transposed[-1, , drop = FALSE]

    ci_transposed %>%
      gt() %>%
      fmt_number(columns = everything(), decimals = 2)
  })

  output$ciPlot <- renderPlot({
    d <- data()
    ci_result <- confidence_result()

    lower <- ci_result$Value[1]
    mean_d <- ci_result$Value[2]
    upper <- ci_result$Value[3]
    se <- (upper - lower) / 2 / qnorm(1 - (1 - input$confidence_level / 100) / 2)

    x_range <- seq(mean_d - 4*se, mean_d + 4*se, length.out = 1000)
    density_vals <- dnorm(x_range, mean = mean_d, sd = se)

    df <- data.frame(x = x_range, y = density_vals)

    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "blue", size = 1.5) +
      geom_vline(xintercept = lower, color = "green", linetype = "dashed", size = 2) +
      geom_vline(xintercept = upper, color = "green", linetype = "dashed", size = 2) +
      geom_area(data = subset(df, x >= lower & x <= upper), aes(x = x, y = y), fill = "lightgray", alpha = 0.5) +
      theme_classic(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        legend.position = "none"
      ) + xlab(" ") + ylab("Density")
  }, height = 400, width = 600)

  output$hypothesisResult <- renderUI({
    d <- data()
    if (is.null(d) || anyNA(d)) return(NULL)

    if (input$sd_known) {
      test_result <- z.test(d, mu = input$mu_0, sigma.x = input$known_sd,
                            alternative = input$alternative, conf.level = 1 - input$alpha)
      test_type <- "Z-test"
      se <- input$known_sd / sqrt(length(d))
      critical_value <- qnorm(1 - input$alpha / 2)
    } else {
      test_result <- t.test(d, mu = input$mu_0, alternative = input$alternative, conf.level = 1 - input$alpha)
      test_type <- "T-test"
      se <- sd(d) / sqrt(length(d))
      critical_value <- qt(1 - input$alpha / 2, df = length(d) - 1)
    }

    decision <- ifelse(test_result$p.value < input$alpha, "Reject the Null Hypothesis", "Fail to Reject the Null Hypothesis")
    power <- 1 - test_result$p.value  # Approximation for educational purposes

    # Create the hypothesis testing summary with centered header
    summary_table <- HTML(paste0(
      "<table border='1' cellpadding='8' cellspacing='0' width='100%' style='border-collapse: collapse; font-family: Arial, sans-serif;'>",
      "<tr style='background-color: #f2f2f2;'><th style='text-align:center; padding: 10px; font-weight: bold;'>Term</th>",
      "<th style='text-align:center; padding: 10px; font-weight: bold;'>Symbol & Value</th>",
      "<th style='text-align:center; padding: 10px; font-weight: bold;'>Definition</th></tr>",

      "<tr><td style='padding: 10px;'>Null Hypothesis</td>",
      "<td style='padding: 10px;'>H<sub>0</sub>: μ = ", input$mu_0, "</td>",
      "<td style='padding: 10px;'>The hypothesis of no effect or difference.</td></tr>",

      "<tr style='background-color: #f9f9f9;'><td style='padding: 10px;'>Alternative Hypothesis</td>",
      "<td style='padding: 10px;'>H<sub>1</sub>: μ ",
      ifelse(input$alternative == "two.sided", "≠ ", ifelse(input$alternative == "greater", "> ", "< ")), input$mu_0,
      "</td><td style='padding: 10px;'>The hypothesis of an effect or difference.</td></tr>",

      "<tr><td style='padding: 10px;'>Significance Level</td>",
      "<td style='padding: 10px;'>α = ", round(input$alpha, 4), "</td>",
      "<td style='padding: 10px;'>The probability of rejecting the null hypothesis when it is true (Type I error).</td></tr>",

      "<tr style='background-color: #f9f9f9;'><td style='padding: 10px;'>Test Statistic</td>",
      "<td style='padding: 10px;'>", test_type, " = ", round(test_result$statistic, 4), "</td>",
      "<td style='padding: 10px;'>A calculated value based on sample data used to determine whether to reject or fail to reject H<sub>0</sub>.</td></tr>",

      "<tr><td style='padding: 10px;'>Critical Value</td>",
      "<td style='padding: 10px;'>", round(critical_value, 4), "</td>",
      "<td style='padding: 10px;'>A value determined by the significance level and the distribution of the test statistic.</td></tr>",

      "<tr style='background-color: #f9f9f9;'><td style='padding: 10px;'>P-value</td>",
      "<td style='padding: 10px;'>", round(test_result$p.value, 4), "</td>",
      "<td style='padding: 10px;'>The probability of obtaining a test statistic as extreme or more extreme than the observed one, assuming H<sub>0</sub> is true.</td></tr>",

      "<tr><td style='padding: 10px;'>Power</td>",
      "<td style='padding: 10px;'>1 - β ≈ ", round(power, 4), "</td>",
      "<td style='padding: 10px;'>The probability of correctly rejecting a false null hypothesis (detecting a true effect).</td></tr>",
      "</table>"
    ))

    decision_text <- ifelse(test_result$p.value < input$alpha,
                            "<p style='color:red; text-align:center; font-weight:bold; font-size: 18px; margin-top: 20px;'>Decision: Reject the Null Hypothesis</p>",
                            "<p style='color:green; text-align:center; font-weight:bold; font-size: 18px; margin-top: 20px;'>Decision: Fail to Reject the Null Hypothesis</p>"
    )

    decision_table <- HTML(paste0(
      "<table border='1' cellpadding='8' cellspacing='0' width='100%' style='border-collapse: collapse; font-family: Arial, sans-serif; margin-top: 20px;'>",
      "<tr style='background-color: #f2f2f2;'><th style='text-align:center; padding: 10px; font-weight: bold;'>Decision</th>",
      "<th style='text-align:center; padding: 10px; font-weight: bold;'>H<sub>0</sub> is True</th>",
      "<th style='text-align:center; padding: 10px; font-weight: bold;'>H<sub>0</sub> is False</th></tr>",

      "<tr><td style='padding: 10px;'>Accept H<sub>0</sub></td>",
      "<td style='padding: 10px;'>True Negative = ", 1 - round(input$alpha, 4), "</td>",
      "<td style='padding: 10px;'>Type II Error = ", round(test_result$p.value, 4), "</td></tr>",

      "<tr style='background-color: #f9f9f9;'><td style='padding: 10px;'>Reject H<sub>0</sub></td>",
      "<td style='padding: 10px;'>Type I Error (α) = ", round(input$alpha, 4), "</td>",
      "<td style='padding: 10px;'>True Positive (Power) = ", round(power, 4), "</td></tr>",
      "</table>"
    ))

    HTML(paste0(summary_table, "<br>", decision_text, "<br>", decision_table))
  })

  output$hypothesisPlot <- renderPlot({
    library(ggplot2)
    library(dplyr)

    d <- data()
    if (is.null(d) || anyNA(d)) return(NULL)

    n <- length(d)
    sample_mean <- mean(d)
    mu_0 <- input$mu_0
    alpha <- input$alpha
    alternative <- input$alternative
    sd_known <- input$sd_known

    if (sd_known) {
      sigma <- input$known_sd
      se <- sigma / sqrt(n)
      z_score <- (sample_mean - mu_0) / se

      # Critical values and p-value
      if (alternative == "two.sided") {
        cv_lower <- mu_0 - qnorm(1 - alpha/2) * se
        cv_upper <- mu_0 + qnorm(1 - alpha/2) * se
        p_value <- 2 * (1 - pnorm(abs(z_score)))
      } else if (alternative == "less") {
        cv <- mu_0 - qnorm(1 - alpha) * se
        p_value <- pnorm(z_score)
      } else {  # "greater"
        cv <- mu_0 + qnorm(1 - alpha) * se
        p_value <- 1 - pnorm(z_score)
      }

      # Generate x values for plotting
      x_min <- mu_0 - 4*se
      x_max <- mu_0 + 4*se
      x_vals <- seq(x_min, x_max, length.out = 1000)
      density_vals <- dnorm(x_vals, mean = mu_0, sd = se)
    } else {
      s <- sd(d)
      se <- s / sqrt(n)
      t_score <- (sample_mean - mu_0) / se
      df <- n - 1

      # Critical values and p-value
      if (alternative == "two.sided") {
        cv_lower <- mu_0 - qt(1 - alpha/2, df) * se
        cv_upper <- mu_0 + qt(1 - alpha/2, df) * se
        p_value <- 2 * (1 - pt(abs(t_score), df))
      } else if (alternative == "less") {
        cv <- mu_0 - qt(1 - alpha, df) * se
        p_value <- pt(t_score, df)
      } else {  # "greater"
        cv <- mu_0 + qt(1 - alpha, df) * se
        p_value <- 1 - pt(t_score, df)
      }

      # Generate x values for plotting
      x_min <- mu_0 - 4*se
      x_max <- mu_0 + 4*se
      x_vals <- seq(x_min, x_max, length.out = 1000)
      density_vals <- dt((x_vals - mu_0)/se, df) / se
    }

    df_plot <- data.frame(x = x_vals, density = density_vals)

    plot <- ggplot(df_plot, aes(x = x, y = density)) +
      geom_line(color = "blue", size = 1.2) +
      geom_vline(xintercept = sample_mean, color = "red", linetype = "solid", size = 1.2) +
      theme_minimal(base_size = 15) +
      xlab("Sample Mean") +
      ylab("Density")

    # Shading regions and adding critical lines
    if (alternative == "two.sided") {
      # Two-sided test: Shade both tails
      plot <- plot +
        geom_area(data = subset(df_plot, x <= cv_lower | x >= cv_upper), aes(y = density), fill = "lightgray", alpha = 0.5) +
        geom_area(data = subset(df_plot, x <= sample_mean | x >= sample_mean), aes(y = density), fill = "yellow", alpha = 0.5) +
        geom_vline(xintercept = c(cv_lower, cv_upper), color = "darkgreen", linetype = "dashed", size = 1.2)
    } else if (alternative == "less") {
      # Left-tailed test: Shade the left side
      plot <- plot +
        geom_area(data = subset(df_plot, x <= cv), aes(y = density), fill = "lightgray", alpha = 0.5) +
        geom_area(data = subset(df_plot, x <= sample_mean), aes(y = density), fill = "yellow", alpha = 0.5) +
        geom_vline(xintercept = cv, color = "darkgreen", linetype = "dashed", size = 1.2)
    } else if (alternative == "greater") {
      # Right-tailed test: Shade the right side
      plot <- plot +
        geom_area(data = subset(df_plot, x >= cv), aes(y = density), fill = "lightgray", alpha = 0.5) +
        geom_area(data = subset(df_plot, x >= sample_mean), aes(y = density), fill = "yellow", alpha = 0.5) +
        geom_vline(xintercept = cv, color = "darkgreen", linetype = "dashed", size = 1.2)
    }

    print(plot)
  }, height = 400, width = 600)



}

list(ui = ui, server = server)
