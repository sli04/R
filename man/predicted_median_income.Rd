\name{predict_median_income}
\alias{predict_median_income}
\title{Predicts Median Income}
\description{
  This function predicts the future median income based on the last 
  10 years of realized data from each province. We use province as a variable and the user
  can predict the future median income based on a province and a future year they want to predict. The
  data is from 2012-2022 so valid years as input would be >=2023. Valid provinces are (AB, BC, MB, NB,   NL, NS, ON, PEI, QC, SK).
\usage{
distribution(province, year)
}
\value{
  A representation of the predicted median income in that inputted year and the linear growth
  rate used to calculate the predicted median income.
}
\examples{
  # Example usage with a dataset 'BC' for British Columbia and year 2035
  predicted_median_income(BC, 2035)
}