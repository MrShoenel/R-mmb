args <- commandArgs(trailingOnly = T)
print(args)

setwd(paste(getwd(), "pkg", "mmb", sep = "/"))

cov <- function() {
  print("Generating coverage report..")
  covr::report(
    x = covr::package_coverage(),
    file = "../../coverage.html",
    browse = T
  )
}


check <- function(strict = T) {
  temp <- devtools::check()
  print(temp)

  cnt <- data.frame(
    err = length(temp$errors),
    war = length(temp$warnings),
    not = length(temp$notes)
  )

  if (cnt$err > 1) {
    stop(paste("check() exited with", cnt$err, "errors."))
  }

  if (strict && sum(cnt) > 0) {
    stop(paste("strict enabled and having one or more warnings/notes."))
  }
}


test <- function() {
  print("Running all unit tests..")
  temp <- data.frame(devtools::test())
  if (sum(temp$failed) > 0) {
    stop(paste(sum(temp$failed), "tests failed."))
  }
}

buildSite <- function() {
  if (file.exists("../../docs")) {
    unlink("../../docs", recursive = T)
  }
  devtools::build_site()
  file.rename("./docs", "../../docs")
  browseURL(normalizePath(paste(getwd(), "../../docs/index.html", sep = "/")))
}


devtools::document()

doAll <- length(args) > 0 & args[1] == "all"
if (doAll) {
  check()
}


test()
cov()


if (doAll) {
  devtools::build()
  devtools::build_manual()
  buildSite()
}
