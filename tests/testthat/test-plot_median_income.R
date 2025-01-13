library(testthat)
library(ggplot2)
library(tidyr)

describe("plot_median_income()", {
  
  it("processes the data correctly and returns a ggplot object", {
    # We use AB as a sample 
    plot <- plot_median_income(AB)
    # Check if the result is a ggplot object
    expect_s3_class(plot, "ggplot")
  })

  it("returns a plot with the correct axis labels and title", {
    # We once again, use AB as a sample 
    plot <- plot_median_income(AB)
    
    # Check axis labels
    expect_equal(plot$labels$x, "Year")
    expect_equal(plot$labels$y, "Median Income ($)")
    
    # Check plot title
    expect_equal(plot$labels$title, "Median Income over Years")
  })
  
  it("processes and pivots data correctly", {
    # We once again, use AB as a sample 
    province_data <- AB
    province_data <- province_data |>
      select(starts_with("20")) |>
      slice(c(15)) |>
      mutate(across(everything(), ~ as.numeric(gsub(",", "", .))))
    
    clean_province <- province_data |>
      pivot_longer(cols = everything(), names_to = "year", values_to = "income")

    expect_true(all(c("year", "income") %in% colnames(clean_province)))
    expect_type(clean_province$income, "double")
  })
})