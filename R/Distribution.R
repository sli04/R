library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

distribution <- function(province) {
  # Removes the median income row (consistent between all data-sets)
  province <- province |>
    slice(-15)
  
  # The latest year in the data-set
  d_year <- max(as.numeric(names(province)[-1]))  # Excludes the first column which is is not data
  
  # Keeps only the selected year based on the latest year in the data-set
  selected_year <- province[, as.character(d_year)] 
  
  # Total only keeps the first row as that is the total number of people who earn an income
  total <- as.numeric(province[1, as.character(d_year)])
  
  # Skips the first row (which is total)
  percentages <- numeric(nrow(province) - 1)  
  
  # Loop that calculates the percentages
  for (i in 2:nrow(province)) {
    row_value <- as.numeric(province[i, as.character(d_year)])
    percentages[i - 1] <- (row_value / total) * 100
  }
  
  # Make a tibble from the for loop output
  result <- tibble(Category = province$'Persons with income 5'[-1], Percentage = percentages)
  
  # Rename each row under the category section to make it more readable
  result <- result |>
    mutate(
      Category = case_when(
        Category == "Persons with income under $5,000 5" ~ "Under $5,000",
        Category == "Persons with income of $5,000 and over 5" ~ "$5,000 and over",
        Category == "Persons with income of $10,000 and over 5" ~ "$10,000 and over",
        Category == "Persons with income of $15,000 and over 5" ~ "$15,000 and over",
        Category == "Persons with income of $20,000 and over 5" ~ "$20,000 and over",
        Category == "Persons with income of $25,000 and over 5" ~ "$25,000 and over",
        Category == "Persons with income of $35,000 and over 5" ~ "$35,000 and over",
        Category == "Persons with income of $50,000 and over 5" ~ "$50,000 and over",
        Category == "Persons with income of $75,000 and over 5" ~ "$75,000 and over",
        Category == "Persons with income of $100,000 and over 5" ~ "$100,000 and over",
        Category == "Persons with income of $150,000 and over 5" ~ "$150,000 and over",
        Category == "Persons with income of $200,000 and over 5" ~ "$200,000 and over",
        Category == "Persons with income of $250,000 and over 5" ~ "$250,000 and over"
      )
    )
  
  # Plot a graph based on the resulting tibble
  distribution_plot <- ggplot(result, aes(reorder(x = Category, -Percentage), y = Percentage)) +
    geom_col(aes(fill = Percentage)) +
    labs(
      x = "Income",
      y = "Percentages",
      title = paste(d_year, "Distribution of Income")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  # Prints the plot
  print(distribution_plot)
  
  # Return the results as a tibble
  return(result)
}
