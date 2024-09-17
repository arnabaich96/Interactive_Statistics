
# 🧠 **Interactive Statistical Concepts** 🧠

## 🎯 **Overview**

This repository contains an interactive statistical document created using *RMarkdown**. The document is designed to help users explore fundamental concepts in statistics through **interactive tools**. These tools allow users to experiment with data, visualize outcomes, and gain insights into various statistical concepts, such as **sampling**, **summary statistics**, **distributions**, **inference**, **ANOVA**, and **Chi-Square tests**.

## 📂 **Contents**

- **Interactive_Statistics.Rmd**: The main R-Markdown file that contains the code and content for the interactive statistical concepts.
- **Preliminaries.R**: R script for the preliminaries section.
- **Summary_Stat.R**: R script for the summary statistics section.
- **Distributions.R**: R script for the distributions section.
- **Inference.R**: R script for the inference section.
- - **Chi-sq_Test.R**: R script for the Chi-Square test section.
- **ANOVA.R**: R script for the ANOVA section.
- **Regression Analysis**: R script for Regression section.

## 🚀 **Getting Started**

### 🔧 **Prerequisites**

To run this project, you need to have the following software installed:

- **R**: The R programming language.
- **RStudio**: An integrated development environment for R.
- **Markdown**: A scientific and technical publishing system built on Pandoc.
- **Shiny**: A web application framework for R (if using interactive components).

### 🛠 **Installation**

1. **Clone the repository**:
   ```bash
   git clone https://github.com/arnabaich96/Interactive_Statistics.git
   ```
   
2. **Navigate to the project directory**:
   ```bash
   cd interactive_statistics
   ```

3. **Install the required R packages**:
   Open the R console and run the following:
   ```r
   install.packages(c("shiny", "plotly", "gtsummary", "gt", "tibble", "shinyjs", "shinyalert", "shinyWidgets", "ggplot2", "datasets", "DT", "BSDA"))
   ```

### 📜 **Instructions for Rendering an RMarkdown Document to HTML**

#### 1. **Rendering in Bash (Linux/macOS) or Terminal (macOS)**

##### Step-by-Step Instructions:

1. **Open Terminal:**
   - On macOS, open the Terminal app.
   - On Linux, open your preferred terminal emulator.

2. **Navigate to the Directory:**
   Use the `cd` command to change to the directory where your RMarkdown file is located.

   ```bash
   cd /path/to/your/rmd/file
   ```

   Replace `/path/to/your/rmd/file` with the actual path to your `.Rmd` file.

3. **Render the Markdown Document to HTML:**
   Use the `Rscript` command to render the RMarkdown file. Here’s the command:

   ```bash
   Rscript -e "rmarkdown::render('Interactive_Statistics.Rmd', output_format = 'html_document')"
   ```

   This command will render the `.Rmd` file and produce an HTML file as output.

4. **Check the Output:**
   The rendered HTML file will be saved in the same directory as the `.Rmd` file.

#### 2. **Rendering in Command Prompt (Windows)**

##### Step-by-Step Instructions:

1. **Open Command Prompt:**
   - Press `Win + R`, type `cmd`, and press `Enter`.

2. **Navigate to the Directory:**
   Use the `cd` command to navigate to the directory where your RMarkdown file is located.

   ```cmd
   cd C:\path\to\your\rmd\file
   ```

   Replace `C:\path\to\your\rmd\file` with the actual path to your `.Rmd` file.

3. **Render the Markdown Document to HTML:**
   Use the `Rscript` command to render the RMarkdown file:

   ```cmd
   Rscript -e "rmarkdown::render('Interactive_Statistics.Rmd', output_format = 'html_document')"
   ```

   This command will render the `.Rmd` file and produce an HTML file as output.

4. **Check the Output:**
   The rendered HTML file will be saved in the same directory as the `.Rmd` file.

## 📌 **General Notes for All Platforms:**

- **Dependencies:** Ensure that R, along with the `rmarkdown` package, is installed on your system.
- **LaTeX for PDF:** If you plan to render to PDF, make sure LaTeX is installed on your system.
- **File Path:** Use the correct path syntax for your operating system (forward slashes `/` for Linux/macOS, backslashes `\` for Windows).

## 🌐 **Deployment**

The document is already deployed to **ShinyApps.io** for interactive exploration. It can be accessed at the following link:

[**Click here to run the application**](https://arnab96.shinyapps.io/interactive_statistics/)

## 💻 **Usage**

Once running, you can view and interact with the statistical tools directly in your browser. Use the provided interactive sliders, inputs, and visualizations to explore various statistical concepts and see how changes in the data affect the results.

## 🌍 **Wiki and Documentation**

Detailed documentation and additional resources are on [Wiki page](https://github.com/arnabaich96/Interactive_Statistics/wiki).

## 📬 **Contact**

For any questions or suggestions, feel free to reach out:

- **Author**: Arnab Aich
- **Email**: [arnab.aich99@gmail.com](mailto:arnab.aich99@gmail.com)
- **GitHub**: [https://github.com/arnabaich96](https://github.com/arnabaich96)
