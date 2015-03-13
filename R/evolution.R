## Use of formula syntax to extract version information from a data.frame
## modeled after LM
evolution <- function(formula, data, subset, diff.fun, ...) {
    if (missing(diff.fun)) {
        stop("diff.fun must be specified")
    }
    Call <- match.call()
    extraArgs <- list(...)
    localCall <- match.call(expand.dots=FALSE)
    argsOrder <- match(c("formula", "data", "subset", "na.action"), names(localCall), 0L)
    localCall <- localCall[c(1L, argsOrder)]
    localCall[[1]] <-quote(stats::model.frame)
    specials <- c("v")
    if (missing(data)) {
        Terms <- terms(formula, specials)
    } else {
        Terms <- terms(formula, specials, data=data)
    }
    stopifnot("v" %in% names(attr(Terms, "specials")))
    mf <- eval(localCall, parent.frame())
    do.call(diff.fun, c(model.response(mf), extraArgs))
}

## Simple version function
## works as a flag to sort the data
## all other terms are simply for other uses
v <- function(x) {
    ## function to signal what should be used as the version
    if (!is.numeric(x)) {
        if (!is.factor(x) || !is.ordered(x)) {
            stop("version term must be either an ordered factor or a numeric vector")
        }
    }
    x
}

## example test code -- incomplete
a <- data.frame(letters=letters, LETTERS=LETTERS, idx = 1:26)
Terms <- terms(letters ~ v(idx), data=a, specials="v")
Terms
model.frame(Terms, a, a$idx<15)

evolDF <- evolution(idx ~ v(idx), data=a, diff.fun=diff)
