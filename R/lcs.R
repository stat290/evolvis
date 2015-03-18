##
#'
#'@export
stringToCharVector <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  unlist(strsplit(x, split=""))
}

## Longest Common Subsequence object constructor (S3-style)
#'
#'@export
lcs <- function(text="", x.index=integer(0), y.index=integer(0)) {
  rtn <- text
  attr(rtn, "x.index") <- as.integer(x.index)
  attr(rtn, "y.index") <- as.integer(y.index)
  class(rtn) <- "lcs"
  rtn
}

## function to concatenate two LCS strings while preserving the indexes
#'
#'@export
combineLcs <- function(lcs1, lcs2) {
  stopifnot(inherits(lcs1, "lcs"), inherits(lcs2, "lcs"))
    lcs(paste(lcs1, lcs2, sep=""),
        c(attr(lcs1, "x.index"), attr(lcs2, "x.index")), 
        c(attr(lcs1, "y.index"), attr(lcs2, "y.index")))
}

## function to compute the LCS from two strings
#'
#'@export
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

## function to rank computed LCS objects on a relative basis
#'
#'@export
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