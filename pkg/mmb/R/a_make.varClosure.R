#' @title Creates a closure over a variable and returns its getter and setter.
#' @author Sebastian Hönel <sebastian.honel@lnu.se>
#' @param initVarVal the initial value of the closed variable.
#' @keywords internal
#' @return list with entries 'get' and 'set' which are getter/setter for the
#' variable that a closure was made over.
make.varClosure <- function(initVarVal = NULL) {
  varClosed <- initVarVal

  return(list(
    get = function() varClosed,
    set = function(val) varClosed <<- val
  ))
}
