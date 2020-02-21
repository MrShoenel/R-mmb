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