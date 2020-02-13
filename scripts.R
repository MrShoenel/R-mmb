args <- commandArgs(trailingOnly = T)
print(args)

setwd(paste(getwd(), "mmb", sep = "/"))

cov <- function() {
  print("Generating coverage report..")
  covr::report(
    x = covr::package_coverage(),
    file = "coverage.html",
    browse = T
  )
}


test <- function() {
  print("Running all unit tests..")
  temp <- data.frame(devtools::test())
  if (sum(temp$failed) > 0) {
    stop(paste(sum(temp$failed), "tests failed."))
  }
}


build <- function() {
  devtools::build()
}


if (length(args) == 0) {
  devtools::document()
  test()
  cov()
  build()
} else {
  print(paste("You have supplied these arguments:", paste(args, collapse = ", ")))
  print("However, arguments are currently ignored.")
}
