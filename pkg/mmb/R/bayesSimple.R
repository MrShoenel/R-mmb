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
  if (nrow(df) == 1 && mmb::getWarnings()) {
    warning("Only one data point given.")
  }
}


#' This function tests for a single nominal label, using the selected features
#' for dependency. It retains at least one value during segmenting and then
#' returns the probability of the target-label (the value of the feature that
#' is the label). In order to determine the most likely label, this function
#' needs to be called once for each candidate.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame
#' @param features data.frame with bayes-features. One of the features needs to
#' be the label-column.
#' @param labelCol string with the name of the feature that represents the label.
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings that
#' are the names of the features the to-predict label depends on. If an empty
#' vector is given, then all of the features are used (except for the label). The
#' order then depends on the features' order.
#' @return double the probability of the target-label, using the maximum a
#' posteriori estimate.
#' @export
bayesInferSimple <- function(df, features, labelCol, selectedFeatureNames = c()) {

  bayesSimpleCheckData(df, features, labelCol)

  if (!(labelCol %in% features$name)) {
    stop("The target-column is not within the features.")
  }

  # Let's filter it out, so that we can handle it separately
  featuresWithoutLabel <- features[!features$isLabel, ]

  if (length(selectedFeatureNames) == 0) {
    if (mmb::getWarnings()) warning("No explicit feature selection, using all.")
    selectedFeatureNames <- features$name[!features$name %in% c(labelCol)]
  }

  # Let's further filter the features to use:
  featuresWithoutLabel <- featuresWithoutLabel[
    featuresWithoutLabel$name %in% selectedFeatureNames, ]

  data <- mmb::conditionalDataMin(
    df, featuresWithoutLabel, selectedFeatureNames, 1)

  # Let's build a frequency table:
  ft <- table(as.character(data[[labelCol]]))

  targetLabel <- as.character(
    mmb::getValueOfBayesFeatures(features[features$isLabel, ], labelCol))

  # Check if target label is in it:
  if (is.na(ft[targetLabel])) {
    return(0) # probability is zero..
  }

  return(ft[[targetLabel]] / sum(ft))
}


#' This function returns the most likely numerical value for the target variable
#' using maximum a posteriori estimation. It retains at least two values for the
#' target variable during segmenting so that an empirical PDF can be estimated.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param df data.frame
#' @param features data.frame with bayes-features. One of the features needs to
#' be the label-column (not required or no value required).
#' @param labelCol string with the name of the feature that represents the label
#' (here the target variable for regression).
#' @param selectedFeatureNames vector default \code{c()}. Vector of strings that
#' are the names of the features the to-predict label depends on. If an empty
#' vector is given, then all of the features are used (except for the label). The
#' order then depends on the features' order.
#' @export
bayesRegressSimple <- function(df, features, labelCol, selectedFeatureNames = c()) {

  bayesSimpleCheckData(df, features, labelCol)

  # Let's filter it out, so that we can handle it separately
  featuresWithoutLabel <- features[!features$isLabel, ]

  if (length(selectedFeatureNames) == 0) {
    selectedFeatureNames <- featuresWithoutLabel$name
  }

  # Let's further filter the features to use:
  featuresWithoutLabel <- featuresWithoutLabel[
    featuresWithoutLabel$name %in% selectedFeatureNames, ]

  data <- mmb::conditionalDataMin(
    df, featuresWithoutLabel, selectedFeatureNames, 2) # note we need 2 for the PDF!

  # Let's build the PDF over the remaining values:
  pdf <- stats::density(data[[labelCol]], bw = "SJ")

  # Return x, so that argmax y = pdf(x)
  return(pdf$x[which.max(pdf$y)])
}

