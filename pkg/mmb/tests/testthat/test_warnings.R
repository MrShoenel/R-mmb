library(testthat)


test_that("closures over variables work", {
  # This test is required explicitly
  vc <- make.varClosure(42)

  expect_equal(vc$get(), 42)
  vc$set(1337)
  expect_equal(vc$get(), 1337)
})


test_that("en-/disabling warnings/errors works", {
  # Check defaults
  expect_true(mmb::getWarnings())

  mmb::setWarnings(F)

  expect_false(mmb::getWarnings())
})
