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
  devtools::test()
}


build <- function() {
  devtools::document()
  devtools::build()
}


if (length(args) == 0) {
  test()
  cov()
  build()
} else {
  print(paste("You have supplied these arguments:", paste(args, collapse = ", ")))
  print("However, arguments are currently ignored.")
}
