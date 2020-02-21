# This is required for the foreach::foreach loops.
utils::globalVariables("featIdx", package = c("mmb"))


#' Computes one single factor that is needed for full Bayesian inferencing.
#' In an equation such as P(A|B) = P(B|A) * P(A) / P(B), the target-feature
#' is A, while the conditional feature is B. There are three factors in that
#' equation (two in the numerator and one in the denominator). This function
#' calculates exactly one factor and expects all features to be given in the
#' right order. If computing the denominator, no target-feature is required.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame with data that is used to segment
#' @param conditionalFeatures data.frame with Bayesian features, as produced
#' by @seealso \code{mmb::createFeatureForBayes()}. This data.frame must not
#' be empty, as we need to depend on at least one feature.
#' @param targetFeature data.frame with exact one Bayesian feature. Any ex-
#' cessive features are discarded and a warning is produced. If computing a
#' factor for the denominator, this data.frame may be empty.
#' @param computeNumerator boolean to indicate whether a factor for the
#' numerator is build. In that case, the target feature is required.
#' @param retainMinValues integer the amount of rows to minimally retain
#' during segmenting using the conditional features.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values.
#' @return numeric the factor as probability or relative likelihood. If the
#' target feature is discrete, a probability is returned; a relative like-
#' lihood, otherwise.
#' @keywords internal
bayesComputeProductFactor <- function(
  df, conditionalFeatures, targetFeature,
  computeNumerator, retainMinValues = 1, doEcdf = F)
{
  if (nrow(conditionalFeatures) == 0) stop("At least one conditional feature is required.")
  if (nrow(targetFeature) > 1 && mmb::getWarnings()) {
    warning("More than one target feature given, taking first, ignoring rest.")
  }

  probFeat <- utils::head(conditionalFeatures, 1) # Always the first conditional feature
  features <- NULL
  if (computeNumerator) {
    if (nrow(targetFeature) == 0) stop("The numerator requires the target-feature.")
    features <- rbind(utils::tail(conditionalFeatures, -1), targetFeature)
  } else {
    features <- utils::tail(conditionalFeatures, -1)
  }

  # In the case we are given a single marginal feature that is
  # not the target feature, segmenting is not required.
  data <- if (nrow(features) == 0) df else mmb::conditionalDataMin(
    df, features, selectedFeatureNames = features$name, retainMinValues)
  fac <- NULL
  if (probFeat$isDiscrete) {
    if (mmb::getMessages()) message(
      paste("Getting conditional probability for", probFeat$name))

    fac <- mmb::getProbForDiscrete(
      data[[probFeat$name]], mmb::getValueOfBayesFeatures(probFeat, probFeat$name))
  } else {
    featVal <- mmb::getValueOfBayesFeatures(probFeat, probFeat$name)
    dataFeature <- data[[probFeat$name]]

    if (doEcdf) {
      if (length(dataFeature) == 0 && mmb::getWarnings()) {
        warning("No data left for ECDF after segmenting.")
        fac <- 0
      } else {
        fac <- stats::ecdf(dataFeature)(featVal)
      }
    } else {
      if (mmb::getMessages()) message(
        paste("Building PDF(", probFeat$name,") given ",
              paste(features$name, collapse = ","), sep = ""))

      dataPdf <- mmb::estimatePdf(dataFeature)

      # If the current value is out of range, then its density is zero anyway, because
      # all of the other values evolve around another range, i.e., the current value
      # is outside the support of the ePDF.
      if (length(dataFeature) == 0 || featVal < dataPdf$min || featVal > dataPdf$max) {
        fac <- 0
      } else {
        fac <- dataPdf$fun(featVal)
      }
    }
  }

  return(fac)
}


#' Computes the probability (discrete feature) or relative likelihood
#' (continuous feature) of one given feature and a concrete value for it.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame that contains all the feature's data
#' @param feature data.frame containing the designated feature as created
#' by @seealso \code{mmb::createFeatureForBayes()}.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values.
#' @return numeric the probability or likelihood of the given feature
#' assuming its given value.
#' @export
bayesComputeMarginalFactor <- function(df, feature, doEcdf = F) {
  prob <- 0
  if (feature$isDiscrete) {
    if (doEcdf && mmb::getWarnings()) {
      warning("Requesting ECDF, but has no effect for discrete variables.")
    }
    prob <- mmb::getProbForDiscrete(
      df[[feature$name]],
      mmb::getValueOfBayesFeatures(feature, feature$name))
  } else {
    featVal <- mmb::getValueOfBayesFeatures(feature, feature$name)
    if (doEcdf) {
      if (nrow(df) == 0) {
        if (mmb::getWarnings()) warning("No data given for estimating the CDF.")
        prob <- 0
      } else {
        prob <- stats::ecdf(df[[feature$name]])(featVal)
      }
    } else {
      pdf <- mmb::estimatePdf(df[[feature$name]])
      prob <- pdf$fun(mmb::getValueOfBayesFeatures(feature, feature$name))
    }
  }
  return(prob)
}



#' Full Bayesian inferencing for determining the probability or relative
#' likelihood of a given value. Uses the full extended theorem of Bayes,
#' taking all selected features into account. Expands
#' Bayes' theorem to accomodate all dependent features, then calculates
#' each conditional probability (or relative likelihood) and returns a
#' single result reflecting the probability or relative likelihood of
#' the target feature assuming its given value, given that all the other
#' dependent features assume their given value. The target feature
#' (designated by 'labelCol') may be discrete or continuous.
#'
#' If at least one of the depending features or the the target feature
#' is continuous and the PDF ('doEcdf' = F) is built, the result of this
#' function is a relative likelihood of the target feature's value. If
#' all of the features are discrete or the empirical CDF is used instead
#' of the PDF, the result of this function is a probability.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame that contains all the feature's data
#' @param features data.frame with bayes-features. One of the features needs
#' to be the label-column.
#' @param targetCol string with the name of the feature that represents the
#' label.
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings
#' that are the names of the features the to-predict label depends on. If an
#' empty vector is given, then all of the features are used (except for the
#' label). The order then depends on the features' order.
#' @param shiftAmount numeric an offset value used to increase any one
#' probability (factor) in the full built equation. In scenarios with many
#' dependencies, it is more likely that a single conditional probability
#' becomes zero, which would result in the entire probability being zero.
#' Since this is often useless, the 'shiftAmount' can be added to each
#' factor, resulting in a non-zero probability that can at least be used
#' to order samples by likelihood. Note that, with a positive 'shiftAmount',
#' the result of this function cannot be said to be a probability any
#' longer, but rather results in a comparable likelihood (a 'probability
#' score').
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values
#' or when doing a regression.
#' @param useParallel default NULL a boolean to indicate whether to use a
#' previously registered parallel backend. If no explicit value was given,
#' calls \code{foreach::getDoParRegistered()} to check for a parallel
#' backend. When using parallelism, this function calculates each factor
#' in the numerator and denominator of the final equation in parallel.
#' @return numeric probability (inferring discrete labels) or relative
#' likelihood (regression, inferring likelihood of continuous value) or most
#' likely value given the conditional features. If using a positive
#' \code{shiftAmount}, the result is a 'probability score'.
#' @export
bayesProbability <- function(
  df, features, targetCol, selectedFeatureNames = c(),
  shiftAmount = 0.1, retainMinValues = 1, doEcdf = F, useParallel = NULL)
{
  bayesSimpleCheckData(df, features, targetCol)

  # One row in features has 'isLabel' = T
  rowOfLabelFeature <- features[features$name == targetCol, ]
  # Let's filter it out, so that we can handle it separately
  featuresWithoutLabel <- features[!features$isLabel, ]

  if (length(selectedFeatureNames) == 0) {
    if (mmb::getMessages()) message("No explicit feature selection, using all.")
    selectedFeatureNames <- featuresWithoutLabel$name
  }

  # Let's further filter the features to use:
  featuresWithoutLabel <- featuresWithoutLabel[
    featuresWithoutLabel$name %in% selectedFeatureNames, ]
  # Now order:
  featuresWithoutLabel <- featuresWithoutLabel[
    match(selectedFeatureNames, featuresWithoutLabel$name), ]

  useParallel <- if (is.logical(useParallel)) useParallel else foreach::getDoParRegistered()
  if (mmb::getMessages()) {
    if (useParallel) {
      message("Using registered parallel backend.")
    } else {
      message("Using sequential computation.")
    }
  }

  foreachOpFun <- if (useParallel) foreach::`%dopar%` else foreach::`%do%`
  computeFac <- function(compNum, featIdx, targetFeat = data.frame()) {
    # Here, we need to slice the features without labels,
    # beginning from the current featIdx.
    condFeats <- featuresWithoutLabel[
      rownames(featuresWithoutLabel) >= featIdx, ]

    return(bayesComputeProductFactor(
      df, condFeats, targetFeat, computeNumerator = compNum,
      retainMinValues = retainMinValues, doEcdf = doEcdf))
  }

  numeratorFactors <- c()
  numeratorFactors <- foreachOpFun(foreach::foreach(
    featIdx = rownames(featuresWithoutLabel),
    .packages = c("mmb", "utils"),
    .combine = c
  ), {
    return(computeFac(T, featIdx, rowOfLabelFeature))
  })

  # Add the marginal:
  numeratorFactors <- c(
    numeratorFactors,
    mmb::bayesComputeMarginalFactor(df, rowOfLabelFeature, doEcdf = doEcdf))

  prodNumerator <- prod(numeratorFactors + shiftAmount)
  if (prodNumerator == 0) {
    if (mmb::getMessages()) message("Numerator was zero, skipping denominator.")
    return(0)
  }

  # Now the denominator:
  denominatorFactors <- c()
  denominatorFactors <- foreachOpFun(foreach::foreach(
    featIdx = rownames(featuresWithoutLabel),
    .packages = c("mmb", "utils"),
    .combine = c
  ), {
    # Here, we do almost the same, except for that we
    # do not require the target feature.
    return(computeFac(F, featIdx))
  })


  prodDenominator <- prod(denominatorFactors + shiftAmount)
  # The following was disabled as it cannot happen in theory and because
  # I was not able to produce a synthetic test that could provoke this.
  #' @seealso test-case "a zero denominator can happen"
  # If you manage to produce such a case please contact me.
  #
  #if (prodDenominator == 0) {
  #  if (mmb::getMessages()) message("Denominator was zero, probability is zero.")
  #  return(0)
  #}

  # OK, both numerator and denominator were > 0!
  return(prodNumerator / prodDenominator)
}
