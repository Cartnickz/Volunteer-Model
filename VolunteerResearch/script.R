#install.packages("tidyverse")
#install.packages("devtools")
#devtools::install_github("dtkaplan/statisticalModeling")
library(tidyverse)
library(ggplot2)
library(statisticalModeling)
library(rpart)

dataset <- read.csv("dataset.csv")


as_tibble(dataset)

dataset$bogus <- rnorm(nrow(dataset)) > 0
dataset$training_cases <- rnorm(nrow(dataset)) > 0

model <- lm(volunteer ~ school_type + extra, data = subset(dataset, training_cases))
model_output <- evaluate_model(model, newdata = subset(dataset, !training_cases))

aug_model <- loess(volunteer ~ school_type + extra, data = subset(dataset, training_cases))
aug_model_output <- evaluate_model(aug_model, newdata = subset(dataset, !training_cases))

model_diff <- with(dataset, volunteer - model_output)
aug_model_diff <- with(dataset, volunteer - aug_model_output)

mean(model_diff ^ 2)
mean(aug_model_diff ^ 2)

fmodel(model, ~ school_type + extra)
