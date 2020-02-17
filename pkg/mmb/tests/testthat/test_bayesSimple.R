library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("invalid arguments in simple bayes lead to warn/stop", {
  expect_does_throw({
    mmb::bayesInferSimple(df = NULL, features = NULL, "bla")
  })
  expect_does_throw({
    mmb::bayesInferSimple(df = iris, features = NULL, "foo")
  })
  expect_does_throw({
    mmb::bayesInferSimple(df = iris[0,], features = data.frame(), "foo")
  })

  features <- sampleToBayesFeatures(iris[1,], "Species")

  expect_does_throw({
    mmb::bayesInferSimple(df = iris[0,], features, "Species")
  })

  expect_does_throw({
    mmb::bayesInferSimple(iris, features, "SpeciesXX")
  })

  w <- mmb::getWarnings()
  mmb::setWarnings(T)
  expect_warning({
    mmb::bayesInferSimple(iris[1,], features, "Species")
  })
  mmb::setWarnings(w)
})


test_that("we can do simple Bayesian inferencing", {
  temp <- iris[iris$Sepal.Length <= 6.1, ]
  cnt <- table(temp$Species)

  labels <- levels(iris$Species)
  featureSep <- mmb::createFeatureForBayes("Sepal.Length", 6.1)

  for (label in labels) {
    featureLab <- mmb::createFeatureForBayes(
      "Species", factor(label, levels = labels), isLabel = T)

    # The probability should be proportional to the results in the above table
    res <- mmb::bayesInferSimple(
      iris, rbind(featureLab, featureSep), "Species", selectedFeatureNames = "Sepal.Length")

    expect_equal(res, cnt[[label]] / sum(cnt), epsilon = 1e-16)
  }
})


test_that("probability is zero if constraints too tight", {
  # Only 5 setosa remain
  temp <- iris[iris$Sepal.Length <= 4.5, ]
  featureSep <- mmb::createFeatureForBayes("Sepal.Length", 4.5)

  labelProbs <- list(
    setosa = 1,
    versicolor = 0,
    virginica = 0
  )

  labels <- levels(iris$Species)
  for (label in labels) {
    featureLab <- mmb::createFeatureForBayes(
      "Species", factor(label, levels = labels), isLabel = T)

    res <- mmb::bayesInferSimple(
      iris, rbind(featureLab, featureSep), "Species", selectedFeatureNames = "Sepal.Length")

    expect_equal(res, labelProbs[[label]], epsilon = 1e-16)
  }
})


test_that("simple Bayesian regression using one or more features work", {
  temp <- iris[iris$Sepal.Length <= 6.1 & iris$Sepal.Width <= 2.9, ]

  featLen <- mmb::createFeatureForBayes("Sepal.Length", 6.1)
  featWid <- mmb::createFeatureForBayes("Sepal.Width", 2.9)

  res <- mmb::bayesRegressSimple(iris, rbind(
    featLen,
    featWid
  ), selectedFeatureNames = c(), labelCol = "Petal.Length")

  # The regression builds the PDF over the remaining Petal.Length and returns argmax
  pdf <- stats::density(temp$Petal.Length, bw = "SJ")

  expect_equal(res, pdf$x[which.max(pdf$y)])
})


