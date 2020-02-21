bayesSimpleCheckData <- function(df, features, labelCol) {
  if (!is.data.frame(df) || !is.data.frame(features)) {
    stop("No data or no features were given.")
  }
  if (nrow(features) <= 1) {
    stop("Need at least one feature as label and one feature to depend on.")
  }
  if (nrow(df) == 0) {
    stop("An empty data.frame was given.")
  }
  if (sum(features$isLabel) > 1) {
    stop(paste("More than one feature selected as label:",
               paste(features$name[features$isLabel], collapse = ", ")))
  }
  if (nrow(df) == 1 && mmb::getWarnings()) {
    warning("Only one data point given.")
  }
}


#' Uses simple Bayesian inference to determine the probability or relative
#' likelihood of a given value. This function can also regress to the most
#' likely value instead. Simple means that segmented data is used in a way
#' that is equal to how a Bayesian network works. For a finite set of labels,
#' this function needs to be called for each, to obtain the probability of
#' each label (or, for n-1 labels or until a label with >.5 probability is
#' found). For obtaining the probability of a continuous value, this function
#' is useful for deciding between picking among a finite set of values. The
#' empirical CDF may be used to obtain an actual probability for a given
#' continuous value, otherwise, the empirical PDF is estimated and a rela-
#' tive likelihood is returned.
#' regression, set \code{doRegress = T} to obtain the most likely value of
#' the target feature, instead of obtaining its relative likelihood.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame
#' @param features data.frame with bayes-features. One of the features needs
#' to be the label-column.
#' @param targetCol string with the name of the feature that represents the
#' label.
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings
#' that are the names of the features the to-predict label depends on. If an
#' empty vector is given, then all of the features are used (except for the
#' label). The order then depends on the features' order.
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @param doRegress default FALSE a boolean to indicate whether to do a
#' regression instead of returning the relative likelihood of a continuous
#' feature. If the target feature is discrete and regression is requested,
#' will issue a warning.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' This parameter does not have any effect when inferring discrete values
#' or when doing a regression.
#' @return numeric probability (inferring discrete labels) or relative
#' likelihood (regression, inferring likelihood of continuous value) or most
#' likely value given the conditional features.
#' @export
bayesInferSimple <- function(
  df, features, targetCol, selectedFeatureNames = c(),
  retainMinValues = 1, doRegress = F, doEcdf = F)
{
  if (doRegress && doEcdf) {
    stop("doRegress and doEcdf are mutually exclusive, only one can be TRUE (or both be FALSE).")
  }

  if (!(targetCol %in% features$name)) {
    if (doRegress) {
      # We allow this case and create a dummy feature.
      features <- rbind(
        features,
        mmb::createFeatureForBayes(targetCol, 0, isLabel = T))
    } else {
      stop("The target-column is not within the features.")
    }
  }

  bayesSimpleCheckData(df, features, targetCol)

  targetFeature <- features[features$isLabel, ]

  # retain minimum amount of values
  retainMinRequired <- if (!targetFeature$isDiscrete && (doRegress || (!doRegress && !doEcdf))) 2 else 0
  if (retainMinValues < retainMinRequired && mmb::getWarnings()) {
    warning("The specified amount of minimum values is too small. Increasing it.")
    retainMinValues <- retainMinRequired
  }
  if (retainMinValues < 1 && mmb::getMessages()) {
    retainMinValues <- 0
    message("retainMinValues set to 0, use with care.")
  }


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



  data <- mmb::conditionalDataMin(
    df, featuresWithoutLabel, selectedFeatureNames, retainMinValues)

  estimate <- 0
  if (targetFeature$isDiscrete) {
    if (doRegress && mmb::getWarnings()) {
      warning("Regression requested but inferring discrete label.")
    }
    estimate <- mmb::getProbForDiscrete(
      data[[targetCol]], mmb::getValueOfBayesFeatures(targetFeature, targetCol))
  } else {
    if (doRegress) {
      # return most likely value
      pdf <- mmb::estimatePdf(data[[targetCol]])
      estimate <- pdf$argmax
    } else {
      featVal <- mmb::getValueOfBayesFeatures(targetFeature, targetCol)

      if (doEcdf) {
        # return the probability to find a value <= featVal
        estimate <- stats::ecdf(data[[targetCol]])(featVal)
      } else {
        # return relative likelihood of given value
        pdf <- mmb::estimatePdf(data[[targetCol]])
        estimate <- pdf$fun(featVal)
      }
    }
  }

  return(estimate)
}



#' Uses simple Bayesian inference to return the probability or relative likeli-
#' hood or a discrete labe or continuous value.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @seealso \code{mmb::bayesInferSimple()}
#' @param df data.frame
#' @param features data.frame with bayes-features. One of the features needs to
#' be the label-column.
#' @param targetCol string with the name of the feature that represents the label.
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings that
#' are the names of the features the to-predict label depends on. If an empty
#' vector is given, then all of the features are used (except for the label). The
#' order then depends on the features' order.
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @param doEcdf default FALSE a boolean to indicate whether to use the
#' empirical CDF to return a probability when inferencing a continuous
#' feature. If false, uses the empirical PDF to return the rel. likelihood.
#' @return double the probability of the target-label, using the maximum a
#' posteriori estimate.
#' @export
bayesProbabilitySimple <- function(
  df, features, targetCol,
  selectedFeatureNames = c(), retainMinValues = 1, doEcdf = F)
{
  return(mmb::bayesInferSimple(
    df, features, targetCol, selectedFeatureNames,
    retainMinValues = retainMinValues, doRegress = F, doEcdf = doEcdf))
}


#' Uses simple Bayesian inferencing to segment the data given the conditional
#' features. Then estimates a density over the remaining values of the target
#' feature and returns the most likely value using a maximum a posteriori
#' estimate of the kernel (returning its mode).
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @seealso \code{mmb::bayesInferSimple()}
#' @param df data.frame
#' @param features data.frame with bayes-features. One of the features needs to
#' be the label-column (not required or no value required).
#' @param targetCol string with the name of the feature that represents the label
#' (here the target variable for regression).
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings that
#' are the names of the features the to-predict label depends on. If an empty
#' vector is given, then all of the features are used (except for the label). The
#' order then depends on the features' order.
#' @param retainMinValues integer to require a minimum amount of data points
#' when segmenting the data feature by feature.
#' @export
bayesRegressSimple <- function(
  df, features, targetCol,
  selectedFeatureNames = c(), retainMinValues = 2)
{
  if (retainMinValues < 2) {
    stop("For regression, retainMinValues must be greater than or equal to 2.")
  }

  return(mmb::bayesInferSimple(
    df, features, targetCol, selectedFeatureNames,
    retainMinValues = retainMinValues, doRegress = T))
}


