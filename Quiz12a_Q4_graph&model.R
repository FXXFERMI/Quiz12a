#### Preamble ####
# Introduction: analysis simulate generate synthetic data for the number of cancer
# related deaths in Sydney's five largest hospitals over a span of 20 years.
# It includes data simulation, model fitting, and plotting.
# Author: Siqi Fei
# Email: fermi.fei@mail.utoronto.ca
# Date: March.28,2024
# Prerequisites:
# 1. R environment.
# 2. Packages:
#    a. tidyverse: For data manipulation and visualization. Install using 
#       install.packages("tidyverse").
#    b. ggplot2: Fot skteching a plot. nstall 
#       using install.packages("ggplot2").
#    c. rstanarm: For building model
#        Install using: install.packages("rstanarm").

#### Work Place Set Up ####
library(ggplot2)
library(tidyverse)
library(rstanarm)

#### Plotting ####
plots <- lapply(hospital_names, function(hospital) {
  subset_data <- data  # Copy the entire data dataframe
  ggplot(subset_data, aes(x = Year, y = .data[[hospital]])) +  # Use .data[[hospital]] to access hospital-specific columns
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = paste("Cancer-Related Deaths in", hospital),
         x = "Year",
         y = "Number of Deaths") +
    theme_minimal()
})

#### Save Plots####
for (i in 1:length(plots)) {
  ggsave(filename = paste0(hospital_names[i], "_deaths.png"), plot = plots[[i]], width = 8, height = 6)
}

#### Data Redefine ####
set.seed(123)
data <- data.frame(
  Year = 2004:2023,
  Hospital1 = round(rnorm(20, mean = 130, sd = 10)),
  Hospital2 = round(rnorm(20, mean = 135, sd = 10)),
  Hospital3 = round(rnorm(20, mean = 140, sd = 10)),
  Hospital4 = round(rnorm(20, mean = 145, sd = 10)),
  Hospital5 = round(rnorm(20, mean = 150, sd = 10)),
  Deaths = round(runif(20, min = 50, max = 150))  # Synthetic outcome variable
)
#### Model ####
formula <- Deaths ~ Year + Hospital1 + Hospital2 + Hospital3 + Hospital4 + Hospital5

# Fit the Bayesian linear regression model using rstanarm
model <- stan_glm(formula, data = data, family = gaussian(), chains = 4)

summary(model)