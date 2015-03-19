##
## Begin sjj code
##

library(evolvis)
library(testthat)

context("XML Parsing of local file")

print(getwd())

# Expect working dir is <package home>/tests/testthat:
load("../../../evolvis/data/small.rda")
parser <- MediaWikiSpecialExportParser()
handler <- XML::xmlEventParse(file=small, branches=parser$saxHandler(), asText=TRUE)
revisions <- parser$details()

test_that("XML parsed objects are as expected", {
  expect_is(revisions, "data.frame")
  expect_equal(sum(dim(revisions) == c(6,6)), 2)
  expect_equal(revisions[1,"pageId"], "1283228")
  expect_equal(sum(nchar(revisions[,6]) == c(716,701,701,710,701,701)), 6)
})

##
## End sjj code
##
