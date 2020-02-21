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


install.mmb <- function() {
  if (base::Sys.getenv("IS_BUILD_COMMAND") != "TRUE") {
    return(0)
  }

  if (!("mmb" %in% rownames(installed.packages()))) {
    buildPath <- base::normalizePath(devtools::build(), mustWork = T)
    install.packages(buildPath, repos = NULL, type = "source")
  }
}

remove.mmb <- function() {
  if (base::Sys.getenv("IS_BUILD_COMMAND") != "TRUE") {
    return(0)
  }

  remove.packages("mmb")
  tryCatch({
    detach("package:mmb", unload = T, character.only = T)
  }, error=function(cond) {})
  base::Sys.unsetenv("IS_BUILD_COMMAND")
}
