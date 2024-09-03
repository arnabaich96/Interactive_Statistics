
# Interactive Statistical Concepts

## Overview

This repository contains an RMarkdown document designed to provide an interactive exploration of key statistical concepts. The document uses Shiny components to allow users to interact with various statistical tools directly within the document. This project is ideal for students, educators, and professionals looking to gain a deeper understanding of statistical methods through hands-on experience.

## Contents

- **Interactive_Statistics.Rmd**: The main RMarkdown file that contains the code and content for the interactive statistical concepts.
- **R/Preliminaries.R**: R script for the preliminaries section.
- **R/Summary_Stat.R**: R script for the summary statistics section.
- **R/Distributions.R**: R script for the distributions section.
- **R/Inference.R**: R script for the inference section.
- **R/ANOVA.R**: R script for the ANOVA section.
- **R/Chi-sq_Test.R**: R script for the Chi-Square test section.

## Prerequisites

To run this project, you need to have the following software installed:

- **R**: The R programming language.
- **RStudio**: An integrated development environment for R.
- **Shiny**: A web application framework for R.
- **Quarto or RMarkdown**: For rendering the document.

## Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/your-github-username/interactive_statistics.git
   ```

2. **Navigate to the project directory**:
   ```bash
   cd interactive_statistics
   ```

3. **Install the required R packages**:
   Open the R console and run the following:
   ```r
   install.packages(c("shiny", "plotly", "gtsummary", "gt", "tibble", "shinyjs", "shinyalert", "shinyWidgets", "ggplot2", "datasets", "DT", "BSDA", "knitr", "kableExtra", "statmod"))
   ```

## Rendering the Document

To render the document and view it interactively:

1. **Open the RMarkdown file** in RStudio.

2. **Knit the Document**:
   - Click the "Knit" button in RStudio.
   - Ensure that the runtime is set to `shiny` to allow the interactive components to function.

3. **View the Output**:
   - The output will be an HTML document with embedded Shiny apps, allowing you to interact with the statistical tools directly in your browser.

## Usage

Once rendered, you can use the interactive elements within the document to explore statistical concepts such as:

- **Sampling and Estimates**: Understand how sample size influences the accuracy of estimates.
- **Summary Statistics**: Explore key summary statistics like mean, median, and standard deviation.
- **Probability Distributions**: Manipulate parameters to see how distributions change shape.
- **Statistical Inference**: Experiment with confidence intervals and hypothesis testing.
- **ANOVA**: Compare the means of three or more groups.
- **Chi-Square Tests**: Test the association between categorical variables.

## Contact

For any questions or suggestions, feel free to reach out:

- **Author**: Arnab Aich
- **Email**: [arnab.aich99@gmail.com](mailto:arnab.aich99@gmail.com)
- **GitHub**: [https://github.com/arnabaich96](https://github.com/arnabaich96)
