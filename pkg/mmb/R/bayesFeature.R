#' Transforms a sample's feature's value into a dataframe, that holds
#' its name, type and value. Currently supports numeric, factor, string
#' and boolean values.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @seealso \code{sampleToBayesFeatures} that uses this function
#' @param name the name of the feature or variable.
#' @param value the value of the feature or variable.
#' @param isLabel default FALSE. Indicates whether this feature or variable
#' is the target variable (the label or value to predict).
#' @param isDiscrete default FALSE. Used to indicate whether the feature or
#' variable given is discrete. This will also be set to true if the value
#' given is a charater, factor or a logical.
#' @return A data.frame with one row holding all the feature's value's
#' properties.
#' @export
createFeatureForBayes <- function(name, value, isLabel = F, isDiscrete = F) {
  df <- data.frame(
    name = name,
    valueNumeric = if (is.numeric(value)) value else NA,
    valueFactor = if (is.factor(value)) value else NA,
    valueString = if (is.character(value)) value else NA,
    valueBool = if (is.logical(value)) value else NA,
    isLabel = isLabel,
    isDiscrete = is.character(value) || is.factor(value) || is.logical(value) || isDiscrete == T,

    stringsAsFactors = F
  )

  if (is.factor(value)) {
    df$valueFactor <- factor(c(as.character(value)), levels = levels(value))
  }

  return(df)
}


#' Internal function to check common arguments for function that
#' use samples transformed to bayes-features.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfFeature a data.frame for a single feature or variable
#' as constructed by @seealso \code{createFeatureForBayes}.
#' @param featName the name of the feature or variable of which to
#' obtain the value.
#' @return data.frame the row corresponding to the given feature name
checkBayesFeature <- function(dfFeature, featName) {
  if (!is.data.frame(dfFeature)) {
    stop("The given dfFeature is not a data.frame.")
  }

  if (!is.character(featName) || nchar(featName) == 0) {
    stop("The given featureName is not a string or it is empty.")
  }

  row <- dfFeature[which(dfFeature$name == featName), ]
  if (nrow(row) == 0) {
    stop("The given featName is not within the data.frame.")
  }

  row
}


#' Given a data.frame with one or multiple features as constructed by
#' @seealso \code{createFeatureForBayes} and a name, extracts the
#' value of the feature specified by name.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfFeature a data.frame for a single feature or variable
#' as constructed by @seealso \code{createFeatureForBayes}.
#' @param featName the name of the feature or variable of which to
#' obtain the value.
#' @return the value of the feature.
#' @export
getValueOfBayesFeatures <- function(dfFeature, featName) {
  row <- checkBayesFeature(dfFeature, featName)

  if (!is.na(row$valueNumeric)) { return(row$valueNumeric) }
  if (!is.na(row$valueFactor)) { return(row$valueFactor) }
  if (!is.na(row$valueString)) { return(row$valueString) }
  if (!is.na(row$valueBool)) { return(row$valueBool) }

  stop("NA values are not supported.")
}


#' Given a data.frame with one or multiple features as constructed by
#' @seealso \code{createFeatureForBayes} and a name, extracts the
#' type of the feature specified by name. Note that this is only
#' used internally.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfFeature a data.frame for a single feature or variable
#' as constructed by @seealso \code{createFeatureForBayes}.
#' @param featName the name of the feature or variable of which to
#' obtain the type.
#' @return the (internal) type of the feature.
#' @export
getValueKeyOfBayesFeatures <- function(dfFeature, featName) {
  row <- checkBayesFeature(dfFeature, featName)

  if (!is.na(row$valueNumeric)) { return("valueNumeric") }
  if (!is.na(row$valueFactor)) { return("valueFactor") }
  if (!is.na(row$valueString)) { return("valueString") }
  if (!is.na(row$valueBool)) { return("valueBool") }

  stop(paste("Cannot determine type of feature:", featName))
}


#' Helper function that takes one sample (e.g., a row of a dataframe
#' with validation data) and transforms it into a data.frame where
#' each row corresponds to one feature (and its value) of the sample.
#' This is done using @seealso \code{createFeatureForBayes}. This
#' operation can be thought of transposing a matrix.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfRow a row of a data.frame with a value for each feature.
#' @param labelCol the name of the feature (column in the data.frame)
#' that is the target variable for classification or regression.
#' @return a data.frame where the first row is the feature that
#' represents the label.
#' @export
sampleToBayesFeatures <- function(dfRow, labelCol) {
  if (!is.data.frame(dfRow) || nrow(dfRow) == 0) {
    stop("Attempted to transform a non- or empty data.frame to a sample.")
  }
  if (!is.character(labelCol) || nchar(labelCol) == 0) {
    stop("The given labelCol is not a string or empty.")
  }
  if (!(labelCol %in% colnames(dfRow))) {
    stop("The given labelCol is not contained in the data.frame.")
  }

  df <- createFeatureForBayes(labelCol, dfRow[[labelCol]], T, T)

  dfRow <- dfRow[!colnames(dfRow) %in% c(labelCol)]
  for (c in colnames(dfRow)) {
    df <- rbind(df, createFeatureForBayes(c, dfRow[[c]]))
  }

  return(df)
}







