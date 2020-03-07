args <- commandArgs(trailingOnly = TRUE)
print(args)

pkgWd <- getwd()
setwd(paste(pkgWd, "pkg", "mmb", sep = "/"))
base::Sys.setenv(IS_BUILD_COMMAND = "TRUE")
source("./tests/helpers.R")


buildEvalVignette <- function(compute = FALSE) {
  oldWd <- getwd()
  setwd(pkgWd)
  # First we render the RMD to markdown. From that markdown, we
  # create additional formats. Otherwise, if using output_format="all",
  # the entire notebook will be evaluated once for every format!

  md <- normalizePath(
    paste("./eval/hyperparameters.md", sep = ""), mustWork = FALSE)
  if (!file.exists(md)) {
    if (compute) {
      warning("Computing notebook, this may take a long time!")

      rmd <- normalizePath(
        paste("./eval/hyperparameters.rmd", sep = ""), mustWork = FALSE)
      rmarkdown::render(
        rmd, output_format = "md_document", clean = TRUE)
    } else {
      # The Notebook is very very heavy and should be rendered outside
      # on some many-core system, it should then be placed here. If this
      # is not yet done, we only create a new, empty markdown, so that
      # the entire process works.
      base::writeLines("", file(md))
    }
  }

  # The next step is to take the ready-rendered MD-version of the
  # RMD Notebook and append it to the stub, then save a copy of
  # that in the vignettes/-dir; also copy the files needed by the MD!
  stub <- normalizePath(
    paste("./eval/hyperparameters-vignette_stub.rmd", sep = ""), mustWork = TRUE)

  vig <- paste(
    paste(base::readLines(stub), collapse = "\n"),
    paste(base::readLines(md), collapse = "\n"),
    sep = "\n\n"
  )

  vigDir <- normalizePath("./pkg/mmb/vignettes", mustWork = FALSE)
  if (!dir.exists(vigDir)) {
    dir.create(vigDir)
  }

  # Write the constructed vignette!
  vigFile <- normalizePath(
    paste(vigDir, "Hyperparameter-Evaluation.rmd", sep = "/"), mustWork = FALSE)
  if (file.exists(vigFile)) {
    file.remove(vigFile)
  }
  base::writeLines(vig, file(vigFile))

  # Copy the MD's files over:
  fileDir <- normalizePath("./eval/hyperparameters_files", mustWork = FALSE)
  fileDirTarget <- normalizePath(
    paste(vigDir, "hyperparameters_files", sep = "/"), mustWork = FALSE)
  if (dir.exists(fileDirTarget)) {
    unlink(fileDirTarget, recursive = TRUE)
  }
  file.copy(fileDir, vigDir, recursive = TRUE)


  # The RMD <-> MD rendering loses the YAML-header, we need it for
  # the other formats.
  mdWithYaml <- normalizePath("./eval/hyperparameters_yaml.md", mustWork = FALSE)
  if (file.exists(mdWithYaml)) {
    file.remove(mdWithYaml)
  }
  base::writeLines(paste(
    paste(base::readLines(
      normalizePath("./eval/hyperparameters.yaml")), collapse = "\n"),
    paste(base::readLines(md), collapse = "\n"),
    sep = "\n\n"
  ), file(mdWithYaml))

  # Let's build the other formats from the md:
  rmarkdown::render(
    mdWithYaml, output_file = rep("hyperparameters", 3),
    output_format = c("html_document", "pdf_document", "word_document"),
    clean = TRUE)

  file.remove(mdWithYaml)

  setwd(oldWd)
}


cov <- function() {
  print("Generating coverage report..")
  covr::report(
    x = covr::package_coverage(),
    file = "../../coverage.html",
    browse = T
  )
}


check <- function(strict = TRUE) {
  temp <- devtools::check(manual = FALSE, document = FALSE)

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
    unlink("../../docs", recursive = TRUE)
  }
  devtools::build_site()
  file.rename("./docs", "../../docs")


  # Last, copy the hyperparameters.html to the articles:
  tryCatch({
    targetDir <- normalizePath(
      paste(pkgWd, "docs", "articles", sep = "/"), mustWork = FALSE)
    if (!dir.exists(targetDir)) {
      dir.create(targetDir, recursive = TRUE)
    }

    temp <- normalizePath(paste(
      pkgWd, "docs", "articles", "hyperparameters.html", sep = "/"),
      mustWork = FALSE)
    if (file.exists(temp)) {
      file.remove(temp)
    }

    file.copy(
      normalizePath(
        paste(pkgWd, "eval", "hyperparameters.html", sep = "/"), mustWork = TRUE),
      temp
    )
  }, error=function(cond){})


  browseURL(normalizePath(paste(getwd(), "../../docs/index.html", sep = "/")))
}


tryCatch({
  doAll <- length(args) > 0 & args[1] == "all"

  remove.mmb()
  devtools::document()

  # Needs to go before check!
  install.mmb() # Builds the package w/o vignettes

  if (doAll) {
    check()
  }

  #test() # testing is done by cov()
  cov()

  if (doAll) {
    devtools::build_readme() # only applies if we have a readme.rmd in the package
    buildEvalVignette()
    devtools::build_vignettes()

    buildSite()
    devtools::build_manual()
  }
}, finally = {
  base::Sys.unsetenv("IS_BUILD_COMMAND")
})


