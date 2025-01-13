# This function calculates the yearly growth of median income and predicts future median income
library(tidyr)
library(readr)
library(dplyr)

predicted_median_income <- function(province, year) {
  
  province <- province |>
    select(starts_with("20")) |>
    # Isolates for median income row
    slice (c(15)) |>
    # Removes any commas
    mutate(across(everything(), ~ as.numeric(gsub(",", "", .))))
  
  years <- as.numeric(names(province))
  # Finds the first year of the data
  first_year <- min(years, na.rm =TRUE)
  # Finds the last year of the data
  last_year <- max(years, na.rm = TRUE)
  
  # Computes the difference from the first and last year
  year_difference <- as.numeric(last_year -first_year)
  
  # Finds the first year median income
  first_year_income <- as.numeric(province[1, which(years == first_year)])
  # Finds the last year median income
  last_year_income <- as.numeric(province[1, which(years == last_year)])
  
  # Computes the difference between the last and first year median income
  income_difference <-(last_year_income - first_year_income)
  # Computes the growth rate
  growth_rate <- income_difference / year_difference

  # Predicts the median income based on user input
  predict <- last_year_income + (year - last_year) * growth_rate

  # Returns as a sentence
  return(paste("The predicted median income for the year", year, "is:", predict, 
               "with a linear growth rate of:",growth_rate))
  
}

