expect_does_throw <- function(expr, doExpect = T) {
  doesThrow <- tryCatch({
    expr
    return(F)
  }, error=function(cond) {
    if (interactive()) warning(paste("Caught:", cond))
    return(T)
  })

  if (doExpect) {
    expect_true(doesThrow)
  }
  return(doesThrow)
}

expect_does_not_throw <- function(expr, doExpect = T) {
  threw <- expect_does_throw(expr, doExpect = F)

  if (doExpect) {
    expect_false(threw)
  }
  return(!threw)
}
