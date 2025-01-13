\name{plot_median_income}
\alias{plot_median_income}
\title{Plots Median Income}
\description{
  This function plots the median income of the inputted province from the
  year 2012-2022. Valid provinces are (AB, BC, MB, NB, NL, NS, ON, PEI, QC, SK).
}
\usage{
plot_median_income(province)
}
\value{
A line plot with years 2012-2022 on the x-axis and median income on the y-axis
}
\examples{
# Example usage with a dataset 'BC' for British Columbia
plot_median_income(BC)
}