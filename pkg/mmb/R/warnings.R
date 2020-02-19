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


varMsg <- make.varClosure(F)


#' Setter for enabling or disabling messages. Messages are disabled by default.
#' Use these to enable high verbosity.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param enable a boolean to indicate whether to enable messages or not.
#' @return void
#' @export
setMessages <- function(enable = T) {
  varMsg$set(!!enable)
  varMsg$get()
}

#' Getter for the state of messages. Returns true if enabled.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @return Boolean to indicate whether messages are enabled or not.
#' @export
getMessages <- function() varMsg$get()
