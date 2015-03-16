## Diff
## http://en.wikipedia.org/wiki/Diff_utility

textDiff <- function(texts) {
  stopifnot(is.character(texts))
  x <- 1:(length(texts) - 1)
  diffs <- lapply(x, FUN=function(x) .singleTextDiff(texts[[x]], texts[[x+1]]))
  currentDiff <- diffs[[1]]
  for (i in 2:length(diffs)) {
    currentDiff <- .mergeTextDiff(currentDiff, diffs[[i]])
  }
  rtn <- .packTextDiff2(currentDiff)
  attr(rtn, "v") <- rtn$firstVersion
  rtn
}

.firstVersion <- function(diffs) {
  n <- length(diffs[[1]])
  findFirstVersion <- function(i, diffs) {
    min(which(!is.na(sapply(diffs, FUN=function(x,i) x[i], i))))
  }
  sapply(1:n, FUN=findFirstVersion, diffs)
}

.diffValues <- function(diffs) {
  n <- length(diffs[[1]])
  findValue <- function(i, diffs) {
    unique(na.omit(sapply(diffs, FUN=function(x,i) x[i], i)))
  }
  sapply(1:n, FUN=findValue, diffs)
}

.diffCodes <- function(diffs) {
  n <- length(diffs[[1]])
  calculateIndexCodes <- function(i, diffs) {
    flags <- unlist(lapply(diffs, 
                           FUN=function(x, i) ifelse(is.na(x[i]), 0, 1), i))
    paste(flags, collapse="")
  }
  sapply(1:n, FUN=calculateIndexCodes, diffs)
}

.packTextDiff <- function(diffs) {
  n <- length(diffs[[1]])
  
  indexCodes <- .diffCodes(diffs)
  indexValues <- .diffValues(diffs)
  firstVersion <- .firstVersion(diffs)
  
  codeStarts <- which(indexCodes[1:(n-1)] != indexCodes[2:n]) + 1
  codeEnds <- codeStarts - 1
  codeStarts <- c(1, codeStarts)
  codeEnds <- c(codeEnds, n)
  
  totalString <- paste(indexValues, collapse="")
  
  substr2 <- function(x, starts, ends, string) {
    substr(string, starts[x], ends[x])
  }
  stringSegments <- sapply(1:length(codeStarts), FUN=substr2,
                           codeStarts, codeEnds, totalString)
  segmentFromIndex <- function(x, starts, ends, segments) {
    segments[x >= starts & x <= ends]
  }
  fullStringSegments <- sapply(1:n, segmentFromIndex,
                               codeStarts, codeEnds, stringSegments)
  segmentTable <- unique(data.frame(indexCodes, fullStringSegments, 
                                    firstVersion, stringsAsFactors=FALSE))
  
  extractVersionData <- function(version, segmentTable) {
    versionContainsSegment <- substr(segmentTable$indexCodes, version, version) == 1
    segmentTable <- segmentTable[versionContainsSegment,]
    numSegments <- sum(versionContainsSegment)
    data.frame(value=segmentTable$fullStringSegments, 
               version = rep(version, times=numSegments),
               element = 1:numSegments,
               firstVersion=segmentTable$firstVersion,
               stringsAsFactors=FALSE)
  }
  
  resultDFs <- lapply(1:length(diffs), extractVersionData, segmentTable)
  do.call("rbind", resultDFs)
}

.mergeTextDiff <- function(x, y) {
  ## right-most string of x must be left-most string of y
  left <- x[[length(x)]]
  right <- y[[1]]
  stopifnot(na.omit(left) == na.omit(right))
  diffIndexes <- .diffIndexFromLcsIndexes(length(left), length(right), 
                                          which(!is.na(left)), 
                                          which(!is.na(right)))
  rtn <- list()
  for (i in 1:length(x)) {
    temp <- as.character(NA, length.out=length(diffIndexes[[1]]))
    temp[!is.na(diffIndexes[[1]])] <- x[[i]][na.omit(diffIndexes[[1]])]
    temp <- list(temp)
    rtn <- c(rtn, temp)
  }
  for (i in 2:length(y)) {
    temp <- as.character(NA, length.out=length(diffIndexes[[2]]))
    temp[!is.na(diffIndexes[[2]])] <- y[[i]][na.omit(diffIndexes[[2]])]
    temp <- list(temp)
    rtn <- c(rtn, temp)
  }
  rtn
}

.singleTextDiff <- function(x, y) {
  stopifnot(is.character(x), length(x) == 1, is.character(y), length(y) == 1)
  lcsObj <- computeLcs(x,y, best.only=TRUE)
  .diffFromLcs(x,y,lcsObj)
}

.diffFromLcs <- function(x, y, lcsObj) {
  stopifnot(is.character(x), length(x) == 1,
            is.character(x), length(y) == 1,
            inherits(lcsObj, "lcs"))
  diffIndexes <- .diffIndexFromLcs(nchar(x), nchar(y), lcsObj)
  xCharVec <- stringToCharVector(x)
  yCharVec <- stringToCharVector(y)
  xRtn <- as.character(rep(NA, length.out=length(diffIndexes[[1]])))
  yRtn <- as.character(rep(NA, length.out=length(diffIndexes[[2]])))
  xRtn[!is.na(diffIndexes[[1]])] <- xCharVec[na.omit(diffIndexes[[1]])]
  yRtn[!is.na(diffIndexes[[2]])] <- yCharVec[na.omit(diffIndexes[[2]])]
  list(xRtn, yRtn)
}

.diffIndexFromLcs <- function(xLength, yLength, lcsObj) {
  stopifnot(is.integer(xLength), is.integer(yLength), inherits(lcsObj, "lcs"))
  .diffIndexFromLcsIndexes(xLength, yLength, 
                           attr(lcsObj, "x.index"), attr(lcsObj, "y.index"))
}

.diffIndexFromLcsIndexes <- function(xLength, yLength, xIndexes, yIndexes) {
  stopifnot(is.integer(xLength), is.integer(yLength), 
            is.integer(xIndexes), is.integer(yIndexes),
            length(xIndexes) == length(yIndexes))
  
  commonLength <- length(xIndexes)
  
  iX <- 1L
  iY <- 1L
  iCommon <- 1L
  rtnX <- integer(0)
  rtnY <- integer(0)
  
  while (iX <= xLength || iY <= yLength) {
    if (iCommon > commonLength) {
      if (iX <= xLength) {
        rtnX <- c(rtnX, iX)
        rtnY <- c(rtnY, NA)
        iX <- iX + 1L
      } else {
        rtnX <- c(rtnX, NA)
        rtnY <- c(rtnY, iY)
        iY <- iY + 1L
      }
    } else if (iX < xIndexes[iCommon]) {
      rtnX <- c(rtnX, iX)
      rtnY <- c(rtnY, NA)
      iX <- iX + 1L
    } else if (iY < yIndexes[iCommon]) {
      rtnX <- c(rtnX, NA)
      rtnY <- c(rtnY, iY)
      iY <- iY + 1L
    } else {
      rtnX <- c(rtnX, iX)
      rtnY <- c(rtnY, iY)
      iX <- iX + 1L
      iY <- iY + 1L
      iCommon <- iCommon + 1L
    }
  }
  list(rtnX, rtnY)
}