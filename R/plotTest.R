##
## plotTest.R
##

## ==== Example 1
evo <- getEvolTestDf1()
vis <- plot(evo) ## method dispatch
## This is a plain old ggvis object, modify as needed...
vis

## ==== Example 2
evo <- getEvolTestDf2()
plot(evo)


## ==== Following are test data generation functions 

## =====================================================================

getEvolTestDf1 <- function() {
  text1 <- "This is a short sentence."
  text2 <- "This is a very short sentence."
  text3 <- "This is a very, very short sentence."
  texts <- c(text1, text2, text3)
  revisionNum <- c(1,2,3)
  authors <- c("Larry", "Curly", "Moe")
  component <- rep("Body", times=3)
  testingData <- data.frame(texts, revisionNum, authors, component,
                            stringsAsFactors=FALSE)
  testingEvol <- evolution(texts ~ v(revisionNum) + authors, 
                           data=testingData, diff.fun=textDiff)
  testingEvol
}

## =====================================================================

getEvolTestDf2 <- function() {
  text1 <- "There was once a leprechaun."
  text2 <- "There was once a saint."
  text3 <- "There was once a saint. He invented green beer."
  text4 <- "There was once a saint. He saved Ireland and invented green beer." 
  texts <- c(text1, text2, text3, text4)
  revisionNum <- c(1,2,3,4)
  authors <- c("Fred", "Sue", "Jack", "Sam")
  component <- rep("Body", times=4)
  testingData <- data.frame(texts, revisionNum, authors, component,
                            stringsAsFactors=FALSE)
  testingEvol <- evolution(texts ~ v(revisionNum) + authors,
                           data=testingData, diff.fun=textDiff)
  testingEvol
}
## ====================================================================
