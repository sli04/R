# The function plots the median income of a province over the years.
library(tidyr)
library(ggplot2)

plot_median_income <- function(province) {
  province <- province |>
    select(starts_with("20")) |>
    # Isolates for median income row
    slice (c(15)) |>
    # Removes any commas
    mutate(across(everything(), ~ as.numeric(gsub(",", "", .))))

  # Cleans the data for plotting
  clean_province <- province |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "income")

  # Plots the cleaned data into a line graph
  plot_income <- ggplot(clean_province, aes(x = year, y = income, group = 1)) +
    geom_line(
      linetype = 1,
      linewidth = 0.5
    ) +
    geom_point(
      color = "deeppink1",
      size = 1
    ) +
    labs(
      y = "Median Income ($)",
      x = "Year",
      title = paste("Median Income over Years")
    ) +
    theme_minimal()
  return(plot_income)
}

