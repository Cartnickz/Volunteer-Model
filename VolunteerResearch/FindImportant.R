library(tidyverse)
library(ggplot2)
library(statisticalModeling)
library(rpart)

# importing dataset
dataset <- read.csv("dataset.csv")

# writing all column names to a list
col_names <- colnames(dataset)

bool_col <- NULL
result_col <- NULL


# sort by columns that are True/False and run a correlation to volunteer
for (name in col_names) {
  if (class(dataset[, name]) == "integer") {
    print(name)
    print(cor(dataset$volunteer, dataset[, name]))
    
    bool_col <- c(bool_col, name)
    result_col <- c(result_col, cor(dataset$volunteer, dataset[, name]))
  }
  cat("\n")
}

# most related are leadership (+), boring (-))
print(bool_col)
print(result_col)
