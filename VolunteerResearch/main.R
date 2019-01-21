#install.packages("stringi")
#install.packages("caTools")
#install.packages("tidyverse")
#install.packages("devtools")
#devtools::install_github("dtkaplan/statisticalModeling")
#install.packages("colorspace")
library(colorspace)
library(tidyverse)
library(ggplot2)
library(statisticalModeling)
library(caret)
library(caTools)

set.seed(101)
# import dataset
dataset <- read.csv("dataset.csv")
dataset$school = NULL

# write all column names to a list
col_names <- colnames(dataset)

# run correlation between all columns with type "integer" and volunteer column
name_col <- NULL 
cor_col <- NULL
for (name in col_names) {
  if (class(dataset[, name]) == "integer") {
    name_col <- c(name_col, name)
    cor_col <- c(cor_col, cor(dataset$volunteer, dataset[, name]))
  }
}

# order variables from greatest coorelation to smallest correlation
sort_num <- NULL 
sort_name <- NULL 

sort_name <- name_col
sort_num <- cor_col
n <- length(sort_num)

print(n)

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

# shuffle dataset and split 60:40
rows <- sample(nrow(dataset))
dataset <- dataset[rows, ]

split <- round(nrow(dataset) * 0.70)
train <- dataset[1:split, ]
test <- dataset[(split+1):nrow(dataset), ]

train$volunteer <- as.factor(train$volunteer)

# run through all combinations of models with significant variables
form_list <- NULL
result_list <- NULL 

sig_name <- sort_name[2:9] # chooses top 10 variables
for (i in 1:length(sig_name)) {
  com <- combn(sig_name, i)
  for (j in 1:ncol(com)) {
    form <- as.formula(paste("volunteer ~", paste(com[,j], collapse = "+")))
    form_list <- c(form_list, form)
    
    model <- train(form, data = train, method = "glm", family = "binomial")
    p <- predict(model, test, type = "prob")

    result <- colAUC(p, test$volunteer, plotROC = TRUE)
    result_list <- c(result_list, result[1])
  }
}

# order models from greatest AUC to lowest AUC
sort_form <- form_list
sort_result <- result_list

n <- length(sort_form)
for(k in n:2) {
  i <- 1
  while (i < k) {
    if (sort_result[i] < sort_result[i + 1]) {
      temp_result <- sort_result[i + 1]
      sort_result[i + 1] <- sort_result[i]
      sort_result[i] <- temp_result
      
      temp_form <- sort_form[i + 1]
      sort_form[i + 1] <- sort_form[i]
      sort_form[i] <- temp_form
    }
    i <- i + 1
  }
}

print(sort_result[1:10])
print(sort_form[1:10])

# use model on test data
model_final <- train(sort_form[[1]], data = train, method = "glm", family = "binomial")
p_final <- predict(model, test, type = "raw")
p_final_prob <- predict(model, test, type = "prob")

str(p_final)
correct <- 0
volunteered <- 0
for (i in 1:length(p_final)) {
  if (p_final[[i]] == train[i, "volunteer"]) {
    correct <- correct + 1
  } 
}

print(correct)

y_n <- ifelse(p_final_prob > 0.30, "Y", "N")
test_con <- ifelse(test[["volunteer"]] == 1, "Y", "N")
print(y_n)
p_class <- factor(y_n[,1], levels = c("Y","N"))
test_class <- factor(test_con, levels = c("Y","N"))

print(test_con)
print(p_class)

levels(test_class)

confusionMatrix(p_class, test_class)

print(length(p_class))
print(length(test_class))

summary(dataset)


