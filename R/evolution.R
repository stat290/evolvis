##
## Begin kjohnsto code
##

#
# evolution.R
#

#' Create an evolution object by differencing source data
#' 
#' Given a \code{data.frame}, extract the data to be differenced, the version
#' to which the data corresponds, and any other relevant characteristics to be
#' maintained with the version.
#' 
#' Formula syntax is used to make data extraction easy. \code{evolution} is 
#' modeled after \code{lm} from the \code{stats} package.
#' 
#' @param formula a formula object to be evaluated within the context of 
#' \code{data}
#' @param data a \code{data.frame} object
#' @param subset a logical vector indicating which subset of \code{data} should
#' be analyzed
#' @param diff.fun a function to do the differencing between elements in the 
#' response to the formula. This function must return an object with an
#' attribute called "v" which links rows with \code{data}. See \code{v} for 
#' details.
#' @param ... additional parameters
#' 
#' @return an \code{evolution} object 
#' 
#' @export
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

#' Version function
#' 
#' A simple version function that operates as a flag for \code{evolution} to
#' sort the data. As differences are order-dependent, \code{evolution} treats
#' this as a special function in the formula and uses the results as the
#' natural ordering of the values.
#' 
#' @param x a value to check for consistency with a version--"orderable"
#' 
#' @return \code{x} if the checks are passed
#' 
#' @export
v <- function(x) {
  ## function to signal what should be used as the version
  if (!is.numeric(x)) {
    if (!is.factor(x) || !is.ordered(x)) {
      stop("version term must be either an ordered factor or a numeric vector")
    }
  }
  x
}
##
## End kjohnsto code
##

