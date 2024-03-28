#### Preamble ####
# Introduction: simulate generate synthetic data for the number of cancer
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

#### Work Place Set Up ####
library(ggplot2)
library(tidyverse)

#### Simulate Data ####
set.seed(123)

years <- 2004:2023
num_hospitals <- 5

data <- data.frame(Year = rep(years, each = num_hospitals))
for (i in 1:num_hospitals) {
  hospital <- paste0("Hospital", i)
  data[hospital] <- round(rnorm(length(years), mean = 130 + 5*i, sd = 10))
}

data_melted <- reshape2::melt(data, id.vars = "Year", variable.name = "Hospital", value.name = "Deaths")

#### Plotting ####
ggplot(data_melted, aes(x = Year, y = Deaths, color = Hospital)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Cancer-Related Deaths in Sydney Hospitals (2004-2023)",
       x = "Year",
       y = "Number of Deaths",
       color = "Hospital") +
  theme_minimal()
