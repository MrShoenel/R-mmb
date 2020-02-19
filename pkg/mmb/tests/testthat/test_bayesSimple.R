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
  expect_warning({
    featPet <- mmb::createFeatureForBayes("Petal.Length", mean(iris$Petal.Length))
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1], isLabel = T)
    mmb::bayesInferSimple(iris, rbind(featPet, featSpe), featSpe$name, doRegress = T)
  })
  mmb::setWarnings(w)

  m <- mmb::getMessages()
  mmb::setMessages(T)
  expect_message({
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
    res <- mmb::bayesInferSimple(iris, featSpe, "Petal.Length", doRegress = T)
  })
  mmb::setMessages(m)

  expect_does_throw({
    # Two labels, Species and Petal.Length will be auto generated.
    featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1], isLabel = T)
    res <- mmb::bayesInferSimple(iris, featSpe, "Petal.Length", doRegress = T)
  })

  expect_does_throw({
    mmb::bayesInferSimple(df = c(1,2,3), data.frame(), "Petal.Length", doRegress = T)
  })

  expect_does_throw({
    mmb::bayesInferSimple(iris, data.frame(), "Petal.Length", doRegress = T)
  })
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
    res <- mmb::bayesProbabilitySimple(
      iris, rbind(featureLab, featureSep), "Species", selectedFeatureNames = "Sepal.Length")

    expect_equal(res, cnt[[label]] / sum(cnt), epsilon = 1e-16)
  }
})


test_that("we can do simple Bayesian inferencing of continuous values", {

  temp <- iris[iris$Species == iris$Species[1] & iris$Petal.Length <= 1.25, ]

  featSpe <- mmb::createFeatureForBayes("Species", iris$Species[1])
  featPet <- mmb::createFeatureForBayes("Petal.Length", 1.25)
  # We want to check how likely this value is given the conditional features,
  # if we plot the conditional density, the most likely value is around ~4.6,
  # so we test 4.6, and that 4 and 5 have a lower relative likelihood
  featSep <- mmb::createFeatureForBayes("Sepal.Length", 0, isLabel = T)

  featSep$valueNumeric <- 4.6
  res_4_6 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name)
  expect_gt(res_4_6, 0.6)
  expect_lt(res_4_6, 0.7)

  featSep$valueNumeric <- 4.0
  res_4_0 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name)
  expect_gt(res_4_0, 0.2)
  expect_lt(res_4_0, 0.3)

  featSep$valueNumeric <- 5.0
  res_5_0 <- mmb::bayesProbabilitySimple(temp, rbind(
    featSpe, featPet, featSep
  ), targetCol = featSep$name)
  expect_gt(res_5_0, 0.4)
  expect_lt(res_5_0, 0.5)
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

    res <- mmb::bayesProbabilitySimple(
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
  ), selectedFeatureNames = c(), targetCol = "Petal.Length")

  # The regression builds the PDF over the remaining Petal.Length and returns argmax
  pdf <- stats::density(temp$Petal.Length, bw = "SJ")

  expect_equal(res, pdf$x[which.max(pdf$y)], epsilon = 1e-15)
})


