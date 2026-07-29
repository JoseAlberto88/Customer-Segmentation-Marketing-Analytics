Customer Segmentation Analysis
================
Elvis Boapeah, Prince Adu Poku, Jose A Martinez Morales
2026-07-29

# Introduction

This notebook conducts a customer segmentation analysis on the
`Purchasing_data.xlsx` data set, following the steps outlined in Part B
of the assignment:

1.  Select segmentation variables
2.  Preprocess the data and run clustering in R
3.  Visualize the segments
4.  Interpret the segments and propose a marketing recommendation

We’ll build this up step by step, one chunk at a time.

# Step 0: Load Packages and Data

``` r
library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)
```

The raw file is a qualtrics export with **two header rows**: the first
row holds short question codes (e.g., `Q03`, `D11`), and the second row
holds the full question text. We read the codes as column names and skip
the text row.

``` r
# Short codes (e.g. "Q03", "D11") become the column names
codes <- read_excel("data/Purchasing_data.xlsx", sheet = "Sheet1", n_max = 0) %>%
  colnames()

# Actual response data starts on row 3, so we skip the first two header rows
raw <- read_excel("data/Purchasing_data.xlsx", sheet = "Sheet1", skip = 2, col_names = FALSE)
colnames(raw) <- codes

dim(raw)
```

    ## [1] 4362   89
