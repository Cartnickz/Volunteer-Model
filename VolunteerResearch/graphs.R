library(tidyverse)
library(forcats)

# Coefficient Graph
cor_data <- data.frame(labels = c("Leadership", 
                             "Sports + Clubs", 
                             "Specialized School",
                             "Friends",
                             "Frustrating"),
                  coef = c(0.144583, 0.128565, 0.115033, 0.082730, -0.11704),
                  p_value = c(1.96E-06, 0.000024, 0.000158, 0.006673, 0.000120806))

plot1 <- ggplot(data = cor_data, aes(x = labels, y = coef, fct_infreq(factor(coef)))) +
  geom_bar(mapping = aes(fill = labels), stat = "identity", show.legend = FALSE) +
  ggtitle("Correlation Coefficient between Explanatory Variables and Response") +
  ylab("Correlation Coefficient") +
  xlab("Explanatory Variable")
  
plot(plot1)

# Mean Plot
mean_data <- read.csv("meanplot.csv")



plot2 <- ggplot(data = mean_data, aes(fill = set, x = label, y = mean, fct_infreq(factor(mean)))) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Avg. Responses between Training and Testing Sets") +
  ylab("% of Yes Responses") +
  xlab("Explanatory Variable")

plot2 
