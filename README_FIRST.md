# Mustafa Asim - Sentiment Analysis Quarto Project

## 1. Put files in one RStudio Project folder
Open RStudio and use the project folder as the working directory.

## 2. Install required R packages
Run this in the Console:

```r
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, tidytext, lubridate, scales, knitr, quarto, tinytex)
tinytex::install_tinytex()
```

## 3. Install the Quarto APA extension
Run this in the Terminal, not in the R Console:

```bash
quarto add wjschne/apaquarto
```

## 4. Render the handout
Run this in the R Console:

```r
quarto::quarto_render("index.qmd", output_format = "html")
quarto::quarto_render("index.qmd", output_format = "apaquarto-pdf")
```

## 5. Render the presentation
Run this in the R Console:

```r
quarto::quarto_render("presentation.qmd", output_format = "revealjs")
quarto::quarto_render("presentation.qmd", output_format = "beamer")
```

Expected outputs:
- index.html
- index.pdf
- presentation.html
- presentation.pdf
- Exercises.zip

## 6. Personal details to edit
In `index.qmd`, replace:
- [your-email]
- [Date]
- [Place]

Add student ID if your professor requires it on the title page.
