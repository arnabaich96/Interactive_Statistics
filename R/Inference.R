library(shiny)
library(plotly)
library(tibble)
library(statmod)  # Load the statmod package for rinvgauss

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(h1("Statistical Inference", align = "center")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      fluidRow(
        column(6, selectInput("dist_type", "Distribution:",
                              choices = c("Normal", "Gamma", "Inverse Gaussian", "t-distribution",
                                          "Chi-squared", "F-distribution", "Binomial", "Uniform"))),
        column(6, numericInput("sample_size", "Sample Size:", value = 500, min = 1))
      ),

      checkboxInput("sd_known", "SD Known", value = FALSE),
      conditionalPanel(
        condition = "input.sd_known == true",
        numericInput("known_sd", "Enter SD:", value = 1)
      ),

      numericInput("confidence_level", "Confidence Level (as %):", value = 95, min = 50, max = 99.99),

      checkboxInput("perform_hypothesis", "Perform Hypothesis Test?", value = FALSE),
      conditionalPanel(
        condition = "input.perform_hypothesis == true",
        fluidRow(
          column(4, numericInput("mu_0", "Null Hypothesis (mu_0):", value = 0)),
          column(4, selectInput("alternative", "Alternative:",
                                choices = c("Equal" = "equal", "More" = "greater", "Less" = "less"))),
          column(4, numericInput("alpha", "Alpha:", value = 0.05, min = 0, max = 1))
        )
      ),

      actionButton("analyze", "Analyze")
    ),

    mainPanel(
      width = 8,
      fluidRow(
        column(12, div(class = "content-box", verbatimTextOutput("confidenceResult"))),
        column(12, div(class = "content-box", plotlyOutput("hypothesisPlot")))
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  observeEvent(input$analyze, {
    data <- reactive({
      if (input$data_entry_type == "File Upload" && !is.null(input$file)) {
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
          file_data <- read.csv(input$file$datapath, header = FALSE)
        } else if (ext == "txt") {
          file_data <- read.table(input$file$datapath, sep = ",", header = FALSE)
        }
        as.numeric(file_data[1, ])
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

    # Confidence Interval Calculation
    confidence_result <- reactive({
      d <- data()
      n <- length(d)
      mean_d <- mean(d)
      if (input$sd_known) {
        sd <- input$known_sd
        se <- sd / sqrt(n)
        z <- qnorm(1 - (1 - input$confidence_level / 100) / 2)
        error_margin <- z * se
        lower <- mean_d - error_margin
        upper <- mean_d + error_margin
        dist_type <- "Z Interval"
      } else {
        sd <- sd(d)
        se <- sd / sqrt(n)
        t <- qt(1 - (1 - input$confidence_level / 100) / 2, df = n - 1)
        error_margin <- t * se
        lower <- mean_d - error_margin
        upper <- mean_d + error_margin
        dist_type <- "T Interval"
      }
      list(mean = mean_d, lower = lower, upper = upper, length = upper - lower, dist_type = dist_type)
    })

    output$confidenceResult <- renderPrint({
      ci <- confidence_result()
      cat("Confidence Interval (", ci$dist_type, "):\n")
      cat("Lower Bound:", round(ci$lower, 4), "\n")
      cat("Midpoint (Mean):", round(ci$mean, 4), "\n")
      cat("Upper Bound:", round(ci$upper, 4), "\n")
      cat("Interval Length:", round(ci$length, 4), "\n")
    })

    # Hypothesis Testing
    hypothesis_result <- reactive({
      if (!input$perform_hypothesis) return(NULL)

      d <- data()
      n <- length(d)
      mean_d <- mean(d)
      mu_0 <- input$mu_0
      if (input$sd_known) {
        sd <- input$known_sd
        se <- sd / sqrt(n)
        z <- (mean_d - mu_0) / se
        p_value <- if (input$alternative == "equal") {
          2 * (1 - pnorm(abs(z)))
        } else if (input$alternative == "greater") {
          1 - pnorm(z)
        } else {
          pnorm(z)
        }
        dist_type <- "Z Test"
        critical_value <- qnorm(1 - input$alpha / 2) * ifelse(input$alternative == "equal", 1, 0) +
          qnorm(1 - input$alpha) * ifelse(input$alternative == "greater", 1, 0) -
          qnorm(input$alpha) * ifelse(input$alternative == "less", 1, 0)
      } else {
        sd <- sd(d)
        se <- sd / sqrt(n)
        t <- (mean_d - mu_0) / se
        p_value <- if (input$alternative == "equal") {
          2 * (1 - pt(abs(t), df = n - 1))
        } else if (input$alternative == "greater") {
          1 - pt(t, df = n - 1)
        } else {
          pt(t, df = n - 1)
        }
        dist_type <- "T Test"
        critical_value <- qt(1 - input$alpha / 2, df = n - 1) * ifelse(input$alternative == "equal", 1, 0) +
          qt(1 - input$alpha, df = n - 1) * ifelse(input$alternative == "greater", 1, 0) -
          qt(input$alpha, df = n - 1) * ifelse(input$alternative == "less", 1, 0)
      }

      decision <- ifelse(
        (input$alternative == "equal" && abs(z) > critical_value) ||
          (input$alternative == "greater" && z > critical_value) ||
          (input$alternative == "less" && z < -critical_value),
        "Reject Null Hypothesis",
        "Fail to Reject Null Hypothesis"
      )

      list(statistic = ifelse(input$sd_known, z, t), p_value = p_value, dist_type = dist_type, decision = decision, critical_value = critical_value)
    })

    output$hypothesisPlot <- renderPlotly({
      hr <- hypothesis_result()
      if (is.null(hr)) return(NULL)

      d <- data()
      density_d <- density(d)

      plot_ly(x = ~density_d$x, y = ~density_d$y, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
        add_segments(x = hr$statistic, xend = hr$statistic, y = 0, yend = max(density_d$y), line = list(color = 'red', name = "Test Statistic")) %>%
        add_segments(x = hr$critical_value, xend = hr$critical_value, y = 0, yend = max(density_d$y), line = list(color = 'orange', dash = 'dash', name = "Critical Value")) %>%
        add_segments(x = hr$p_value, xend = hr$p_value, y = 0, yend = max(density_d$y), line = list(color = 'green', name = "P-Value")) %>%
        layout(
          legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
          shapes = list(
            list(
              type = "rect",
              x0 = ifelse(input$alternative == "less", min(density_d$x), hr$critical_value),
              x1 = ifelse(input$alternative == "greater", max(density_d$x), hr$critical_value),
              y0 = 0, y1 = max(density_d$y),
              fillcolor = "lightgray", opacity = 0.5, line = list(width = 0)
            )
          ),
          yaxis = list(title = "Density"),
          xaxis = list(title = "Value")
        )
    })
  })
}

shinyApp(ui, server)
