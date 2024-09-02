
library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Distribution Calculator"),

  sidebarLayout(
    sidebarPanel(
      width = 3,  # Narrower sidebar

      # Distribution selection
      selectInput("dist_type", "Select Distribution:",
                  choices = c("Normal", "Chi-squared", "t-distribution", "F-distribution")),

      # Side-by-side radio buttons for Calculation Type and Tail
      fluidRow(
        column(6, radioButtons("calc_type", "Calculation Type:",
                               choices = c("Probability", "Quantile"), inline = TRUE)),
        column(6, radioButtons("side", "Tail:",
                               choices = c("Lower", "Upper"), inline = TRUE))
      ),

      # Conditional inputs based on calculation type
      conditionalPanel(
        condition = "input.calc_type == 'Probability'",
        numericInput("point", "Point:", value = 0)
      ),

      conditionalPanel(
        condition = "input.calc_type == 'Quantile'",
        numericInput("probability", "Probability:", value = 0.5)
      ),

      # Side-by-side inputs for distribution parameters
      conditionalPanel(
        condition = "input.dist_type == 'Normal'",
        fluidRow(
          column(6, numericInput("normal_mean", "Mean:", value = 0)),
          column(6, numericInput("normal_sd", "Standard Deviation:", value = 1))
        )
      ),

      conditionalPanel(
        condition = "input.dist_type == 'Chi-squared'",
        numericInput("chisq_df", "Degrees of Freedom:", value = 10)
      ),

      conditionalPanel(
        condition = "input.dist_type == 't-distribution'",
        numericInput("t_df", "Degrees of Freedom:", value = 10)
      ),

      conditionalPanel(
        condition = "input.dist_type == 'F-distribution'",
        fluidRow(
          column(6, numericInput("f_df1", "Degrees of Freedom 1:", value = 10)),
          column(6, numericInput("f_df2", "Degrees of Freedom 2:", value = 10))
        )
      ),

      # Action button for calculation
      actionButton("calculate", "Calculate")
    ),

    mainPanel(
      width = 9,  # Wider main panel for plot
      uiOutput("dynamicTitle"),  # Dynamic panel heading
      plotlyOutput("distPlot", height = "500px")  # Larger plot area
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$calculate, {
    x <- NULL
    y <- NULL
    highlight_x <- NULL
    highlight_y <- NULL
    prob <- NULL
    quantile <- NULL
    panel_heading <- NULL
    plot_title <- NULL

    if (input$calc_type == "Probability") {
      # Probability calculation
      if (input$dist_type == "Normal") {
        mean <- input$normal_mean
        sd <- input$normal_sd
        point <- input$point

        x <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)
        y <- dnorm(x, mean, sd)

        if (input$side == "Lower") {
          highlight_x <- x[x <= point]
          highlight_y <- dnorm(highlight_x, mean, sd)
          prob <- pnorm(point, mean, sd)
        } else {
          highlight_x <- x[x >= point]
          highlight_y <- dnorm(highlight_x, mean, sd)
          prob <- 1 - pnorm(point, mean, sd)
        }
        quantile <- point
        panel_heading <- paste("Probability =", round(prob, 4))
        plot_title <- paste("Density Curve of Normal(", mean, ",", sd, ")")

      } else if (input$dist_type == "Chi-squared") {
        df <- input$chisq_df
        point <- input$point

        x <- seq(0, 4 * df, length.out = 1000)
        y <- dchisq(x, df)

        if (input$side == "Lower") {
          highlight_x <- x[x <= point]
          highlight_y <- dchisq(highlight_x, df)
          prob <- pchisq(point, df)
        } else {
          highlight_x <- x[x >= point]
          highlight_y <- dchisq(highlight_x, df)
          prob <- 1 - pchisq(point, df)
        }
        quantile <- point
        panel_heading <- paste("Probability =", round(prob, 4))
        plot_title <- paste("Density Curve of Chi-squared(", df, ")")

      } else if (input$dist_type == "t-distribution") {
        df <- input$t_df
        point <- input$point

        x <- seq(-4, 4, length.out = 1000)
        y <- dt(x, df)

        if (input$side == "Lower") {
          highlight_x <- x[x <= point]
          highlight_y <- dt(highlight_x, df)
          prob <- pt(point, df)
        } else {
          highlight_x <- x[x >= point]
          highlight_y <- dt(highlight_x, df)
          prob <- 1 - pt(point, df)
        }
        quantile <- point
        panel_heading <- paste("Probability =", round(prob, 4))
        plot_title <- paste("Density Curve of t(", df, ")")

      } else if (input$dist_type == "F-distribution") {
        df1 <- input$f_df1
        df2 <- input$f_df2
        point <- input$point

        x <- seq(0, 4, length.out = 1000)
        y <- df(x, df1, df2)

        if (input$side == "Lower") {
          highlight_x <- x[x <= point]
          highlight_y <- df(highlight_x, df1, df2)
          prob <- pf(point, df1, df2)
        } else {
          highlight_x <- x[x >= point]
          highlight_y <- df(highlight_x, df1, df2)
          prob <- 1 - pf(point, df1, df2)
        }
        quantile <- point
        panel_heading <- paste("Probability =", round(prob, 4))
        plot_title <- paste("Density Curve of F(", df1, ",", df2, ")")
      }

    } else if (input$calc_type == "Quantile") {
      # Quantile calculation
      if (input$dist_type == "Normal") {
        mean <- input$normal_mean
        sd <- input$normal_sd
        prob <- input$probability

        quantile <- qnorm(prob, mean, sd)
        x <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 1000)
        y <- dnorm(x, mean, sd)

        if (input$side == "Lower") {
          highlight_x <- x[x <= quantile]
        } else {
          highlight_x <- x[x >= quantile]
        }
        highlight_y <- dnorm(highlight_x, mean, sd)
        panel_heading <- paste("Quantile =", round(quantile, 4))
        plot_title <- paste("Density Curve of Normal(", mean, ",", sd, ")")

      } else if (input$dist_type == "Chi-squared") {
        df <- input$chisq_df
        prob <- input$probability

        quantile <- qchisq(prob, df)
        x <- seq(0, 4 * df, length.out = 1000)
        y <- dchisq(x, df)

        if (input$side == "Lower") {
          highlight_x <- x[x <= quantile]
        } else {
          highlight_x <- x[x >= quantile]
        }
        highlight_y <- dchisq(highlight_x, df)
        panel_heading <- paste("Quantile =", round(quantile, 4))
        plot_title <- paste("Density Curve of Chi-squared(", df, ")")

      } else if (input$dist_type == "t-distribution") {
        df <- input$t_df
        prob <- input$probability

        quantile <- qt(prob, df)
        x <- seq(-4, 4, length.out = 1000)
        y <- dt(x, df)

        if (input$side == "Lower") {
          highlight_x <- x[x <= quantile]
        } else {
          highlight_x <- x[x >= quantile]
        }
        highlight_y <- dt(highlight_x, df)
        panel_heading <- paste("Quantile =", round(quantile, 4))
        plot_title <- paste("Density Curve of t(", df, ")")

      } else if (input$dist_type == "F-distribution") {
        df1 <- input$f_df1
        df2 <- input$f_df2
        prob <- input$probability

        quantile <- qf(prob, df1, df2)
        x <- seq(0, 4, length.out = 1000)
        y <- df(x, df1, df2)

        if (input$side == "Lower") {
          highlight_x <- x[x <= quantile]
        } else {
          highlight_x <- x[x >= quantile]
        }
        highlight_y <- df(highlight_x, df1, df2)
        panel_heading <- paste("Quantile =", round(quantile, 4))
        plot_title <- paste("Density Curve of F(", df1, ",", df2, ")")
      }
    }

    output$dynamicTitle <- renderUI({
      h3(panel_heading, align = "center")
    })

    output$distPlot <- renderPlotly({
      plot_ly(x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Density") %>%
        add_trace(x = ~highlight_x, y = ~highlight_y, fill = "tozeroy", name = "Area Under Curve") %>%
        add_lines(x = c(quantile, quantile), y = c(0, max(y)), line = list(color = 'red', width = 4), name = "Quantile Line") %>%
        layout(
          title = plot_title,
          xaxis = list(title = "X"),
          yaxis = list(title = "Density"),
          showlegend = FALSE
        )
    })
  })
}

shinyApp(ui, server)
