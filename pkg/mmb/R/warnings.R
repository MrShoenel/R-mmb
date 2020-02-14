varWarn <- make.varClosure(T)


#' Setter for enabling or disabling warnings. Warnings are enabled by default.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param enable a boolean to indicate whether to enable warnings or not.
#' @return void
#' @export
setWarnings <- function(enable = T) {
  varWarn$set(!!enable)
  varWarn$get()
}

#' Getter for the state of warnings. Returns true if enabled.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @return Boolean to indicate whether warnings are enabled or not.
#' @export
getWarnings <- function() varWarn$get()
