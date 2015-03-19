##
## Begin sjj code
##

library(evolvis)
library(testthat)

context("Longest Common Subsequence")

## Example from wikipedia page
## http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

lcsA <- lcs()
lcsB <- lcs("b", 1, 4)
lcsAB <- combineLcs(lcsA,lcsB)

test_that("LCS object is as expected", {
  expect_is(lcsA, "lcs")
  expect_is(lcsB, "lcs")
  expect_equal(lcsB[[1]], "b")
  expect_equal(attr(lcsB, "x.index"), 1)
  expect_equal(attr(lcsB, "y.index"), 4)
  expect_equal(lcsAB[[1]], "b")
  expect_equal(attr(lcsAB, "x.index"), 1)
  expect_equal(attr(lcsAB, "y.index"), 4)
})

##
## End sjj code
##

# TODO More tests to formalize: 

# computeLcs("GAC", "AGCAT")
# computeLcs("XMJY .AUZ", "MZJ .AWXU")
# x <- "This is a sentence."
# y <- "This is a very short sentence."
# exampleLCS <- computeLcs(x, y)


# longIndex <- attr(sequences, "y.index")
# longIndex
# y.mod <- unlist(strsplit(y, split=""))
# 1:length(y.mod)
# textDiff(x,y)
# is.integer(attr(sequences, "x.index"))
# is.numeric(attr(sequences, "x.index"))
