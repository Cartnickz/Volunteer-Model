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
summary(dataset)

model <- lm(volunteer ~  leadership, data = subset(dataset, training_cases))

summary(model)
fmodel(model, ~leadership)
ggplot(data = dataset) +
  geom_point(mapping = aes(x = leadership, y = volunteer, size = volunteer))
model_output <- evaluate_model(model, data = subset(dataset, !training_cases))

print(model_output)

aug_model <- lm(volunteer ~ help_others + leadership, data = subset(dataset, training_cases))
aug_model_output <- evaluate_model(aug_model, data = subset(dataset, !training_cases))

model_diff <- with(dataset, volunteer - model_output$volunteer)
aug_model_diff <- with(dataset, volunteer - aug_model_output$volunteer)


mean(model_diff ^ 2)
mean(aug_model_diff ^ 2)


