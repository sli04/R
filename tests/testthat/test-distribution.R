library(testthat)

describe("distribution()", {
  it("processes the data correctly and returns a tibble", {
    # We use AB as a sample
    result <- distribution(AB)
    # Check if the result contains the expected columns
    expect_true(all(c("Category", "Percentage") %in% colnames(result)))
    # Check that Percentage values are numeric
    expect_type(result$Percentage, "double")
  })
  it("stops the code if invalid province code is entered",{
    expect_error(distribution(qweq))
    expect_error(distribution(BCS))
  })
})