library(testthat)

describe("predict_median_income()", {
  it("can print to the console", {
    expect_output(cat(predicted_median_income(AB, 213123)))
  })
  it("handles valid inputs correctly", {
    result <- predicted_median_income(AB, 2030)
    expect_type(result, "character")
  })
  it("returns error for invalid province data", {
    expect_error(predicted_median_income(NULL, 2030))
  })
  it("returns error for invalid year", {
    expect_error(predicted_median_income(AB, hehehethisiswrong))
  })
})