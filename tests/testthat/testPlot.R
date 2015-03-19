##
## Begin sjj code
##
library(evolvis)
library(testthat)

context("Generate visualization")

text1 <- "This is a short sentence."
text2 <- "This is a very short sentence."
text3 <- "This is a very, very short sentence."
texts <- c(text1, text2, text3)
revisionNum <- c(1,2,3)
authorZZZ <- c("Larry", "Curly", "Moe")
component <- rep("Body", times=3)

testData <- data.frame(texts, revisionNum, authorZZZ, component,
                          stringsAsFactors=FALSE)
testEvo <- evolution(texts ~ v(revisionNum) + authorZZZ, 
                         data=testData, diff.fun=textDiff)
vis <- plot(testEvo)

test_that("Evolution and visualization objects are as expected", {
  expect_is(testEvo, "evolution")
  expect_is(testEvo, "data.frame")
  expect_is(vis, "ggvis")
  expect_equal(sum(dim(testEvo) == c(9,5)), 2)
  expect_equal("plot.evolution" %in% methods(plot), TRUE)
  expect_equal(length(vis), 14)
})
##
## End sjj code
##

# TODO More test scenarios to formalize:

# text1 <- "There was once a leprechaun." 
# text2 <- "There was once a wolf."
# text3 <- "There was once a wolf. He invented green beer."
# text4 <- "There was once a wolf. He saved Ireland and invented green beer." 
# texts <- c(text1, text2, text3, text4)
# revisionNum <- c(1,2,3,4)
# Irish_Historians <- c("Fred", "Sue", "Jack", "Sam")
# component <- rep("Body", times=4)
# testingData <- data.frame(texts, revisionNum, Irish_Historians, component,
#                           stringsAsFactors=FALSE)
# testingEvol <- evolution(texts ~ v(revisionNum) + Irish_Historians,
#                          data=testingData, diff.fun=textDiff)
# testingEvol


