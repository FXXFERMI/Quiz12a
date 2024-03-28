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
#    c. testthat: For testing Dataset
#        Install using: install.packages("testthat").

#### Work Place Set Up ####
library(ggplot2)
library(tidyverse)
library(testthat)

#### Simulate Data ####
set.seed(123)

num_years <- 20
num_hospitals <- 5

data <- data.frame(Year = 2004:2023)

set.seed(123)


num_years <- 20
num_hospitals <- 5

# Create a dataframe to store the data
data <- data.frame(Year = 2004:2023)
hospital_names <- paste0("Hospital", 1:num_hospitals)
data[hospital_names] <- sapply(1:num_hospitals, function(i) {
  round(rnorm(num_years, mean = 130 + 5*i, sd = 10))
})

#### Test ####
test_that("Dataset Test Cases", {
  
  # Test 1: Check if the number of rows is correct
  expect_equal(nrow(data), 20)
  
  # Test 2: Check if the number of columns is correct
  expect_equal(ncol(data), 6)  # Year + 5 Hospitals
  
  # Test 3: Ensure that all values in the 'Year' column are unique
  expect_equal(length(unique(data$Year)), 20)
  
  # Test 4: Ensure that all hospital names are correctly generated
  expected_hospital_names <- paste0("Hospital", 1:5)
  expect_equal(colnames(data)[-1], expected_hospital_names)
  
  # Test 5: Ensure that the number of deaths is non-negative for all data points
  expect_true(all(data[, -1] >= 0))
  
  # Test 6: Check if the structure of the dataset matches the expected structure
  expect_named(data, c("Year", expected_hospital_names))
  
  # Test 7: Check if the 'Year' column is of numeric data type
  expect_is(data$Year, "integer")
  
  # Test 8: Check if the hospital columns are of integer data type
  for (hospital in expected_hospital_names) {
    expect_is(data[[hospital]], "numeric")
  }
  
  # Test 9: Ensure that there are no missing values in the dataset
  expect_false(any(is.na(data)))
  
  # Test 10: Ensure that the data is in the expected format (data frame)
  expect_is(data, "data.frame")
})