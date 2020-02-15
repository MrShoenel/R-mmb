#' Takes a data.frame and segments it, according to the selected variables.
#' Only rows satisfying all conditions are kept. Supports discrete and con-
#' tinuous variables.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @seealso \code{getValueKeyOfBayesFeatures()}
#' @param df data.frame with data to segment. If it contains less than or
#' equally many rows as specified by \code{retainMinValues}, then the same
#' data.frame is returned.
#' @param features data.frame of bayes-features that are used to segment.
#' Each feature's value is used to segment the data, and the features are
#' used in the order as given by \code{selectedFeatureNames}. If those are
#' not given, then the order of this data.frame is used.
#' @param selectedFeatureNames default \code{c()}. Character vector with the
#' names of the variables that shall be used for segmenting. Segmenting is
#' done variable by variable, and the order depends on this vector. If this
#' vector is empty, then the originally given data.frame is returned.
#' @param retainMinValues default 1. The minimum amount of rows to retain.
#' Filtering the data by the selected features may reduce the amount of
#' remaining rows quickly, and this can be used as an early stopping criteria.
#' Note that filtering is done variable by variable, and the amount of
#' remaining rows is evaluated after each segmenting-step. If the threshold
#' is undercut, then the result from the previous round is returned.
#' @return data.frame that is segmented according to the selected variables
#' and the minimum amount of rows to retain.
#' @export
conditionalDataMin <- function(df, features, selectedFeatureNames = c(), retainMinValues = 1) {
  if (length(selectedFeatureNames) == 0 || nrow(df) <= retainMinValues) {
    if (mmb::getWarnings()) warning("No features selected or data.frame too small.")
    return(df)
  }

  features <- features[features$name %in% selectedFeatureNames, ]
  filteredData <- df

  for (i in 1:length(selectedFeatureNames)) {
    fIdx <- which(features$name == selectedFeatureNames[i])
    feature <- features[fIdx, ]
    op <- if (feature$isDiscrete) " == " else " <= "
    val <- paste(
      "features[", fIdx, ', "',
      getValueKeyOfBayesFeatures(feature, feature$name),
      '"]', sep = "")

    temp <- filteredData[eval(parse(text =
      paste('(filteredData[["', feature$name, '"]]', op, val, ")", sep = ""))), ]
    if (nrow(temp) < retainMinValues) {
      # Premature stop, warn about this.
      if (mmb::getWarnings()) warning("Segmenting stopped prematurely.")
      return(filteredData)
    } else {
      filteredData <- temp
    }
  }

  filteredData
}
