##
## Begin sjj code
##

library(evolvis)
library(testthat)

context("Differencing")

x <- "This is a sentence."
y <- "This is a very short sentence."
z <- "This is a very, very short sentence."
exampleLCS <- computeLcs(x, y)

test_that("LCS object is as expected", {
  expect_is(exampleLCS, "lcs")
  expect_is(exampleLCS[[1]], "character")
  expect_equal(nchar(exampleLCS[[1]]), 19)
  expect_equal(length(attr(exampleLCS, "x.index")), 19)
  expect_equal(length(attr(exampleLCS, "y.index")), 19)
})

d1 <- evolvis:::.singleTextDiff(x, y)
d2 <- evolvis:::.singleTextDiff(y, z)
d3 <- evolvis:::.mergeTextDiff(d1,d2)

test_that("Text difference and merge as expected", {
  expect_is(d1, "list")
  expect_is(d2, "list")
  expect_equal(length(d1[[1]]), 30)
  expect_equal(length(d2[[1]]), 36)
  expect_equal(sum(is.na(d2[[1]])), 6)
  expect_is(d3, "list")
  expect_equal(length(d3), 3)
  expect_equal(sum(is.na(d3[[1]])), 17)
})

##
## End sjj code
##

# TEST MORE tests to formalize: 

# diffObj <- textDiff(c(x,y,z))
# str(diffObj)
# diffObj

# evolvis:::.diffIndexFromLcs(nchar(x), nchar(y), exampleLCS)
# evolvis:::.diffFromLcs(x, y, exampleLCS)

# texts <- attr(diffObj, "text")
# paste(texts[unlist(diffObj[1,])], collapse="") == x
# paste(texts[unlist(diffObj[2,])], collapse="") == y
# paste(texts[unlist(diffObj[3,])], collapse="") == z

