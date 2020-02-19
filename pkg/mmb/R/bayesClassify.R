# This is required for the foreach::foreach loops.
utils::globalVariables("featIdx", package = c("mmb"))


#' Creates a string that can be used in Latex in an e.g. equation-environment.
#' @note Use \code{cat()} to print a string that can be copy-pasted.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param conditionalFeatures data.frame of Bayesian features, the target-
#' feature depends on.
#' @param targetFeature data.frame that holds exactly one Bayesian feature,
#' that is supposed to be the target-feture for Bayesian inferencing.
#' @param includeValues default FALSE boolean to indicate whether to include
#' the features' values or not, i.e. "A" vs. "A = setosa".
#' @return a string that can be used in Latex documents.
#' @export
bayesToLatex <- function(conditionalFeatures, targetFeature, includeValues = F) {
  if (nrow(conditionalFeatures) == 0 || nrow(targetFeature) == 0) {
    stop("Need exactly one target-feature and one or more conditional features.")
  }

  if (nrow(targetFeature) > 1) {
    if (mmb::getWarnings()) warning("More than one target feature given, taking first, ignoring rest.")
    # We only allow one target feature:
    targetFeature <- utils::head(targetFeature, 1)
  }

  # Left side of equation, e.g. "P(A|B,C) = "
  ltx <- paste("P(\\textit{", targetFeature$name, "} | ", paste(sapply(conditionalFeatures$name, function(name) {
    return(paste("\\textit{", name, "}", sep = ""))
  }), collapse = ","), ") = ", sep = "")

  ltxFeatVal <- function(feat) {
    val <- mmb::getValueOfBayesFeatures(feat, feat$name)
    if (feat$isDiscrete) {
      val <- paste("\\text{", val, "}", sep = "")
    }
    return(val)
  }
  ltxFeatFac <- function(feat) {
    val <- paste("\\textit{", feat$name, "}", sep = "")
    if (includeValues) {
      op <- if (feat$isDiscrete) "=" else "\\leq"
      val <- paste(val, op, ltxFeatVal(feat))
    }
    return(val)
  }

  ltxNumerator <- sapply(1:nrow(conditionalFeatures), function(condFeatIdx) {
    isLast <- condFeatIdx == utils::tail(nrow(conditionalFeatures), 1)
    feat <- conditionalFeatures[condFeatIdx, ]
    fact <- paste("P(", ltxFeatFac(feat), " | ", sep = "")

    others <- NULL
    if (isLast) {
      others <- rbind(targetFeature)
    } else {
      nextIdx <- if (isLast) condFeatIdx else condFeatIdx + 1
      others <- rbind(conditionalFeatures[nextIdx:nrow(conditionalFeatures), ],
                      targetFeature)
    }

    others <- sapply(1:nrow(others), function(rn) {
      return(ltxFeatFac(others[rn, ]))
    })

    return(paste(fact, paste(others, collapse = ","), ")", sep = ""))
  })
  ltxNumerator <- c(ltxNumerator, paste("P(", ltxFeatFac(targetFeature), ")", sep = ""))

  ltxDenominator <- sapply(1:nrow(conditionalFeatures), function(condFeatIdx) {
    isLast <- condFeatIdx == utils::tail(nrow(conditionalFeatures), 1)
    feat <- conditionalFeatures[condFeatIdx, ]
    fact <- paste("P(", ltxFeatFac(feat), sep = "")

    if (isLast) {
      return(paste(fact, ")", sep = ""))
    }

    others <- conditionalFeatures[(condFeatIdx + 1):nrow(conditionalFeatures), ]
    others <- sapply(1:nrow(others), function(rn) {
      return(ltxFeatFac(others[rn, ]))
    })

    return(paste(fact, " | ", paste(others, collapse = ","), ")", sep = ""))
  })


  paste(ltx, "\\frac{",
        paste(ltxNumerator, collapse = " \\times "),
        "}{",
        paste(ltxDenominator, collapse = " \\times "),
        "}", sep = "")
}


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
      # all of the other values evolve around another range.
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

