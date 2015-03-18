## Use of formula syntax to extract version information from a data.frame
## modeled after LM
## diff.fun must return an object with an attribute called "v" which links
##    rows up with the original version
#'
#'@export
evolution <- function(formula, data, subset, diff.fun, ...) {
  if (missing(diff.fun)) {
    stop("diff.fun must be specified")
  }
  Call <- match.call()
  extraArgs <- list(...)
  localCall <- match.call(expand.dots=FALSE)
  specials <- c("v")  
  if (missing(data)) {
    mt <- terms(formula, specials)
  } else {
    mt <- terms(formula, specials, data=data)
  }
  stopifnot("v" %in% names(attr(mt, "specials")))
  argsOrder <- match(c("formula", "data", "subset"), 
                     names(localCall), 0L)
  localCall <- localCall[c(1L, argsOrder)]
  localCall[[1]] <- quote(stats::model.frame)
  localCall[[2]] <- mt
  mf <- eval(localCall, parent.frame())
  columnsToExclude <- c(attr(mt, "response"), unlist(attr(mt, "specials")))
  
  ## sort the model frame by version number
  mf <- mf[order(mf[[attr(mt,"specials")$v]]), ]
  diffs <- do.call(diff.fun, list(model.response(mf)))
  rtn <- cbind(diffs, mf[attr(diffs, "v"),-columnsToExclude, drop=FALSE], stringsAsFactors=FALSE)
  attr(rtn, "terms") <- attr(mf, "terms")
  class(rtn) <- c("evolution", class(rtn))
  rtn
}

## Simple version function
## works as a flag to sort the data
## all other terms are simply for other uses
#'
#'@export
v <- function(x) {
  ## function to signal what should be used as the version
  if (!is.numeric(x)) {
    if (!is.factor(x) || !is.ordered(x)) {
      stop("version term must be either an ordered factor or a numeric vector")
    }
  }
  x
}
