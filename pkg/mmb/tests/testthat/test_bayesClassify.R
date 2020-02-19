library(testthat)

source("../helpers.R")

library(datasets)
data("iris")


test_that("conversion to latex-equations work", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  cf <- rbind(
    mmb::createFeatureForBayes("B", round(mean(df$B), 2)),
    mmb::createFeatureForBayes("C", round(mean(df$C), 2))
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = T)

  w <- mmb::getWarnings()
  mmb::setWarnings(T)

  expect_warning({
    mmb::bayesToLatex(tf, cf)
  })

  expect_does_throw({
    mmb::bayesToLatex(data.frame(), tf)
  })
  expect_does_throw({
    mmb::bayesToLatex(cf, data.frame())
  })


  expect_equal(mmb::bayesToLatex(cf, tf, F), "P(\\textit{A} | \\textit{B},\\textit{C}) = \\frac{P(\\textit{B} | \\textit{C},\\textit{A}) \\times P(\\textit{C} | \\textit{A}) \\times P(\\textit{A})}{P(\\textit{B} | \\textit{C}) \\times P(\\textit{C})}")
  expect_equal(mmb::bayesToLatex(cf, tf, T), "P(\\textit{A} | \\textit{B},\\textit{C}) = \\frac{P(\\textit{B} \\leq 5.84 | \\textit{C} \\leq 1.2,\\textit{A} = \\text{setosa}) \\times P(\\textit{C} \\leq 1.2 | \\textit{A} = \\text{setosa}) \\times P(\\textit{A} = \\text{setosa})}{P(\\textit{B} \\leq 5.84 | \\textit{C} \\leq 1.2) \\times P(\\textit{C} \\leq 1.2)}")

  mmb::setWarnings(w)
})


test_that("the factors for products are built correctly", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  cf <- rbind(
    mmb::createFeatureForBayes("B", mean(df$B)),
    mmb::createFeatureForBayes("C", mean(df$C))
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = T)

  w <- mmb::getWarnings()
  mmb::setWarnings(T)
  m <- mmb::getMessages()
  mmb::setMessages(T)

  expect_does_throw({
    bayesComputeProductFactor(df, data.frame(), tf, computeNumerator = T)
  })
  expect_does_throw({
    bayesComputeProductFactor(df, cf, data.frame(), computeNumerator = T)
  })
  expect_does_not_throw({
    expect_message({
      bayesComputeProductFactor(df, cf, data.frame(), computeNumerator = F)
    })
  })
  expect_warning({
    bayesComputeProductFactor(df, tf, cf, computeNumerator = T)
  })

  # Let's filter iris and let B's dist. depend on C,A
  filtered <- df[df$C <= mean(df$C) & df$A == df$A[1], ]
  # Emp. PDF of B, segmented on C=c, A=a
  dens <- approxfun(stats::density(filtered$B, bw = "SJ"))
  # Returns P(B|C,A)
  bayesRegressVal <- bayesComputeProductFactor(df, cf, tf, computeNumerator = T)

  expect_equal(bayesRegressVal, dens(cf$valueNumeric[1]), tolerance = 1e-15)


  # Ask for a value out of range for B:
  cf[1,]$valueNumeric <- 0.1
  expect_equal(bayesComputeProductFactor(df, cf, tf, computeNumerator = T), 0)

  mmb::setWarnings(w)
  mmb::setMessages(m)
})


test_that("using the ecdf works as well", {
  df <- data.frame(
    A = iris$Species,
    B = iris$Sepal.Length,
    C = iris$Petal.Width
  )

  tf <- mmb::createFeatureForBayes("A", df$A[1], isLabel = T)
  cf <- mmb::createFeatureForBayes("B", mean(df$B))

  temp <- df[df$A == df$A[1],]
  res <- bayesComputeProductFactor(df, cf, tf, computeNumerator = T, doEcdf = T)
  expect_equal(res, ecdf(temp$B)(cf$valueNumeric), tolerance = 1e-10)

  # Let's do too aggressive segmenting
  cf <- rbind(cf, mmb::createFeatureForBayes("C", min(df$C - 1e-10)))
  res <- expect_warning({
    bayesComputeProductFactor(df, cf, tf, computeNumerator = T,
                              doEcdf = T, retainMinValues = 0)
  })
  expect_equal(res, 0)
})


test_that("marginal factors are calculated correctly", {
  expect_equal(mmb::bayesComputeMarginalFactor(
    iris, mmb::createFeatureForBayes("Species", iris$Species[1])), 1/3, tolerance = 1e-10)

  w <- mmb::getWarnings()
  mmb::setWarnings(T)

  expect_warning({
    # ecdf has no effect for discrete
    mmb::bayesComputeMarginalFactor(
      iris, mmb::createFeatureForBayes("Species", iris$Species[1]), doEcdf = T)
  })

  expect_warning({
    # no data
    mmb::bayesComputeMarginalFactor(
      iris[0,], mmb::createFeatureForBayes("Petal.Length", 1337), doEcdf = T)
  })

  mmb::setWarnings(w)


  # Let's do some things that should definitely work!
  df <- data.frame(
    data = rnorm(25000)
  )

  res <- mmb::bayesComputeMarginalFactor(
    df, mmb::createFeatureForBayes("data", 0), doEcdf = TRUE)

  expect_equal(res, 0.5, tolerance = 1e-2)

  res <- mmb::bayesComputeMarginalFactor(
    df, mmb::createFeatureForBayes("data", 0), doEcdf = FALSE)

  expect_equal(
    res, stats::approxfun(stats::density(df$data, bw = "SJ"))(0), tolerance = 1e-15)
})



