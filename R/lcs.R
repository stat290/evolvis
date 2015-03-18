#
# lcs.R
#

#' Convert a string into a vector of characters
#'
#' Instead of using string-specific operations to address substrings, it can be
#' easier to expand one string into a vector of characters for addressing and
#' manipulation. Largely, this exists as a convenience function for computing
#' the LCS, but it has some merit in other forms as well. The function checks
#' the size and class of \code{x} before performing the splitting operation.
#' 
#' @param x a single character string, to be split
#' @param divider a token to use to divide \code{x}
#' 
#' @return a character vector with the substrings of  \code{x}.
#' 
#' @export
stringToCharVector <- function(x, divider="") {
  stopifnot(is.character(x), length(x) == 1)
  unlist(strsplit(x, split=divider))
}


#' Constructor for a Longest Common Subsequence (LCS) object
#'
#' The longest common subsequence of two strings is an important step in the
#' diff algorithm.
#' 
#' @param text the string value of the LCS
#' @param x.index the indexes of the first string that match the characters in
#' the LCS
#' @param y.index the indexes of the second string that match the characters in
#' the LCS
#' 
#' @return a string with two attributes--one for \code{x.index} and one for
#' \code{y.index}
#' 
#' @export
lcs <- function(text="", x.index=integer(0), y.index=integer(0)) {
  rtn <- text
  attr(rtn, "x.index") <- as.integer(x.index)
  attr(rtn, "y.index") <- as.integer(y.index)
  class(rtn) <- "lcs"
  rtn
}


#' Concatenate two LCS objects
#'
#' Given two LCS objects, concatenate them in the order given.
#' 
#' @param lcs1 the first LCS
#' @param lcs2 the second LCS
#' 
#' @return an LCS object in which the strings, \code{x.index}, and 
#' \code{y.index} have each been concatenated with the corresponding pair.
#' 
#' @export
combineLcs <- function(lcs1, lcs2) {
  stopifnot(inherits(lcs1, "lcs"), inherits(lcs2, "lcs"))
    lcs(paste(lcs1, lcs2, sep=""),
        c(attr(lcs1, "x.index"), attr(lcs2, "x.index")), 
        c(attr(lcs1, "y.index"), attr(lcs2, "y.index")))
}

#' Compute the Longest Common Subsequence (LCS) for two strings
#'
#' Given two LCS objects, calculate the longest common subsequences
#' 
#' @param x the first string
#' @param y the second string
#' @param best.only a logical value indicating whether only the "best" LCS
#' should be returned ("best" is determined by relative quality, see 
#' \code{lcsQuality})
#' 
#' @return one or more an LCS object(s) which represent the longest common
#' subsequence to \code{x} and \code{y}
#' 
#' @export
computeLcs <- function(x,y, best.only=TRUE) {
  stopifnot(is.character(x), length(x) == 1, 
            is.character(y), length(y) == 1)  
  ## convert x and y to single-character vectors
  x <- stringToCharVector(x)
  y <- stringToCharVector(y)
  
  resultLength <- .prepLCSMatrix(x,y)
  listArray <- .prepLCSArray(x,y)
    
  sequences <- .lcsAlgorithm(resultLength, listArray, x, y)
  if (best.only) {
    qualityScores <- sapply(sequences, lcsQuality)
    sequences <- sequences[[which.max(qualityScores)]]
  }
  sequences
}

#' Measure the quality of an LCS object
#'
#' Quality represents a relative measure of how good an LCS object is to
#' another. For any two strings, it is possible that a large number of LCS 
#' objects can be equally valid. This function measures the quality of the 
#' object by the length of the continuous sequences in the indexes. Equal weight
#' is given to both \code{x.index} and \code{y.index}.
#' 
#' @param x an LCS object
#' 
#' @return a number signifying how good the LCS is (larger numbers being better)
#' for relative rankings of LCS object
#' 
#' @export
lcsQuality <- function(x) {
  stopifnot(inherits(x, "lcs"))
  sum(c(.lcsIndexQuality(attr(x, "x.index")),
        .lcsIndexQuality(attr(x, "y.index"))))
}

.lcsIndexQuality <- function(x) {
  stopifnot(is.integer(x))
  innerBreakpoints <- which(c(0, diff(x), length(x) + 1) != 1)
  outerBreakpoints <- c(1, innerBreakpoints, length(x))
  squaredSequenceSizes <- sum(diff(outerBreakpoints) ^ 2)
  squaredSequenceSizes
}

.prepLCSMatrix <- function(x, y) {
    resultLength <- matrix(nrow=length(x)+1, ncol=length(y)+1)
    resultLength[1,] <- 0
    resultLength[,1] <- 0
    resultLength
}

.prepLCSArray <- function(x, y) {
    lcsArray <- list()
    for (i in 1:(length(x)+1)) {
        lcsArray[[i]] <- list()
        for (j in 1:(length(y)+1)) {
            lcsArray[[i]][[j]] <- list(lcs())
        }
    }
    lcsArray
}

.lcsAlgorithm <- function(resultLength, listArray, x, y) {
  numRows <- dim(resultLength)[1]
  numCols <- dim(resultLength)[2]
  
  rowNames <- c("0", x)
  colNames <- c("0", y)
    
  for (i in 2:numRows) {
    for (j in 2:numCols) {
      if (rowNames[i] == colNames[j]) {
        resultLength[i,j] <- resultLength[i-1, j-1] + 1
        unit <- lcs(rowNames[i], i-1, j-1)
        listArray[[i]][[j]] <- lapply(listArray[[i-1]][[j-1]], combineLcs, unit)
      } else {
        up <- resultLength[i-1, j]
        left <- resultLength[i, j-1]
        resultLength[i,j] <- max(up, left)
        if (up > left) {
          listArray[[i]][[j]] <- listArray[[i-1]][[j]]
        } else if (left > up) {
          listArray[[i]][[j]] <- listArray[[i]][[j-1]]
        } else {
          listArray[[i]][[j]] <- c(listArray[[i-1]][[j]], listArray[[i]][[j-1]])
        }
      }
    }
  }
  listArray[[numRows]][[numCols]]
}