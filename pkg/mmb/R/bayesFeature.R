#' Transforms a sample's feature's value into a dataframe, that holds
#' its name, type and value. Currently supports numeric, factor, character
#' and boolean values. Note that factor is internally converted to
#' character.
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
createFeatureForBayes <- function(name, value, isLabel = FALSE, isDiscrete = FALSE) {
  value <- if (is.factor(value)) as.character(value) else value

  df <- data.frame(
    name = name,
    valueNumeric = if (is.numeric(value)) value else NA,
    valueChar = if (is.character(value)) value else NA,
    valueBool = if (is.logical(value)) value else NA,
    isLabel = isLabel,
    isDiscrete = is.character(value) || is.logical(value) || isDiscrete == TRUE,
    isNumeric = is.numeric(value),
    isCharacter = is.character(value),
    isLogical = is.logical(value),

    stringsAsFactors = FALSE
  )

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
    stop("The given featureName is not character or it is empty.")
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

  if (row$isNumeric) { return(row$valueNumeric) }
  if (row$isCharacter) { return(row$valueChar) }
  if (row$isLogical) { return(row$valueBool) }

  stop(paste("Corrupted feature without allowed value:", featName))
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

  if (row$isNumeric) { return("valueNumeric") }
  if (row$isCharacter) { return("valueChar") }
  if (row$isLogical) { return("valueBool") }

  stop(paste("Corrupted feature without discernible type:", featName))
}


#' Helper function that takes one sample (e.g., a row of a dataframe
#' with validation data) and transforms it into a data.frame where
#' each row corresponds to one feature (and its value) of the sample.
#' This is done using @seealso \code{createFeatureForBayes}. This
#' operation can be thought of transposing a matrix.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfRow a row of a data.frame with a value for each feature.
#' @param targetCol the name of the feature (column in the data.frame)
#' that is the target variable for classification or regression.
#' @return a data.frame where the first row is the feature that
#' represents the label.
#' @export
sampleToBayesFeatures <- function(dfRow, targetCol) {
  if (!is.data.frame(dfRow) || nrow(dfRow) == 0) {
    stop("Attempted to transform a non- or empty data.frame to a sample.")
  }
  if (!is.character(targetCol) || nchar(targetCol) == 0) {
    stop("The given targetCol is not character or empty.")
  }
  if (!(targetCol %in% colnames(dfRow))) {
    stop("The given targetCol is not contained in the data.frame.")
  }

  # Ensure compat.
  df <- mmb::bayesConvertData(dfRow)
  df <- createFeatureForBayes(targetCol, dfRow[[targetCol]], T)

  dfRow <- dfRow[!colnames(dfRow) %in% c(targetCol)]
  for (c in colnames(dfRow)) {
    df <- rbind(df, createFeatureForBayes(c, dfRow[[c]]))
  }

  return(df)
}


#' Counter operation to @seealso \code{mmb::sampleToBayesFeatures()}.
#' Takes a Bayes-feature data.frame and transforms it back to a row.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param dfOrg data.frame containing at least one row of the original
#' format, so that we can rebuild the sample matching exactly the
#' original column names.
#' @param features data.frame of Bayes-features, as for example
#' previously created using \code{mmb::sampleToBayesFeatures()}.
#' @return data.frame the sample as 1-row data.frame.
#' @export
bayesFeaturesToSample <- function(dfOrg, features) {
  if (!is.data.frame(dfOrg) || !is.data.frame(features)) {
    stop("Reference data.frame or features is not a data.frame.")
  }
  if (nrow(dfOrg) == 0 || nrow(features) == 0) {
    stop("Reference data.frame or features are empty.")
  }

  cols <- colnames(dfOrg)
  sample <- data.frame(matrix(nrow = 1, ncol = length(cols)))
  colnames(sample) <- cols

  for (c in cols) {
    val <- mmb::getValueOfBayesFeatures(features, c)
    if (is.factor(dfOrg[[c]])) {
      sample[, c] <- factor(val, levels = levels(dfOrg[[c]]))
    } else {
      sample[, c] <- val
    }
  }

  return(sample)
}
