#install.packages("tidyverse")
#install.packages("devtools")
#devtools::install_github("dtkaplan/statisticalModeling")
library(tidyverse)
library(ggplot2)
library(statisticalModeling)

# import dataset
dataset <- read.csv("dataset.csv")

# write all column names to a list
col_names <- colnames(dataset)

# run correlation between all columns with type "integer" and volunteer column
name_col <- NULL
cor_col <- NULL

for (name in col_names) {
  if (class(dataset[, name]) == "integer") {
    name_col <- c(bool_col, name)
    cor_col <- c(cor_col, cor(dataset$volunteer, dataset[, name]))
  }
}

# order variables from greatest coorelation to smallest correlation
sort_name <- name_col
sort_num <- cor_col

n <- length(sort_num)
for(k in n:2) {
  i <- 1
  while (i < k) {
    if (abs(sort_num[i]) < abs(sort_num[i + 1])) {
      temp_num <- sort_num[i + 1]
      sort_num[i + 1] <- sort_num[i]
      sort_num[i] <- temp_num
      
      temp_name <- sort_name[i + 1]
      sort_name[i + 1] <- sort_name[i]
      sort_name[i] <- temp_name
    }
    i <- i + 1
  }
}

# run through all combinations of models with significant variables
sig_name <- sort_name[2:6] # chooses top 5 variables

for (i in 1:length(sig_name)) {
  com <- combn(sig_name, i)
  for (j in 1:ncol(com)) {
    form <- as.formula(paste("volunteer ~", paste(com[,j], collapse = "+")))
    model <- lm (form, data = dataset)
    summary(model)
  }
}