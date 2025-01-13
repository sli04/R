\name{distribution}
\alias{distribution}
\title{Distribution of Income}
\description{
  This function outputs a tibble with the distribution of income w.r.t the inputted province.
  The distribution is respective to the latest year in the data-set. Furthermore, the function
  then outputs a column graph of the distribution of income. Valid provinces are (AB, BC, MB, NB, NL, NS, ON, PEI, QC, SK).
}
\usage{
distribution(province)
}
\value{
Outputs a tibble and a column graph
}
\examples{
# Example usage with a dataset 'BC' for British Columbia
distribution(BC)
}