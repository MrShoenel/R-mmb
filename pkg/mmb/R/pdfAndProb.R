#' Given a few observations of a random variable, this function returns an
#' approximation of the PDF as a function.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @note If the given vector is empty, warns and returns a constant function
#' that always returns zero for all values.
#' @note If the given vector contains only one observation, then a function
#' is returned that returns 1 iff the value supplied is the same as the
#' observation. Otherwise, that function will return zero.
#' @param data vector of numeric data. Used to compute the empirical density
#' of the data.
#' @param densFun function default \code{stats::density} with bandwith 'SJ'.
#' Function to compute the empirical density of a non-empty vector of numerical
#' data.
#' @return list with a function that is the empirical PDF using KDE. It
#' also has two properties 'min' and 'max' which represent the integratable
#' range of that function. 'min' and 'max' are both zero if not data (an
#' empty vector) was given. If one data point was given, then they correspond
#' to its value -/+ \code{.Machine$double.eps}.
#' @export
estimatePdf <- function(data = c(), densFun = function(vec) {
  stats::density(vec, bw = "SJ")
}) {
  l <- length(data)
  pdf <- list(
    fun = NULL,
    min = 0,
    max = 0
  )

  if (l == 0) {
    if (mmb::getWarnings()) warning("No data was given for the PDF.")
    pdf$fun <- function(x) 0
  } else if (l == 1) {
    if (mmb::getWarnings()) warning("Only one data point given for estimating the PDF.")
    pdf$fun <- function(x) {
      if (x == data[1]) {
        return(1)
      }
      return(0)
    }
    pdf$min <- data[1] - .Machine$double.eps
    pdf$max <- data[1] + .Machine$double.eps
  } else {
    densFun <- densFun(data)
    pdf$fun <- stats::approxfun(densFun)
    pdf$min <- min(densFun$x)
    pdf$max <- max(densFun$x)
  }


  return(pdf)
}



#' Similar to @seealso \code{estimatePdf}, this function returns the probability
#' for a discrete value, given some observations.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @note If no observations are given, then this function will warn and return
#' a probability of zero for the value given. While we could technically return
#' positive infinity, 0 is more suitable in the context of Bayesian inferencing.
#' @param data vector of observations that have the same type as the given value.
#' @param value a single observation of the same type as the data vector.
#' @return the probability of value given data.
#' @export
getProbForDiscrete <- function(data, value) {
  if (length(data) == 0) {
    if (mmb::getWarnings()) warning("No data was given for calculating probability for discrete value.")
    return(0)
  }

  return(sum(data == value) / length(data))
}
