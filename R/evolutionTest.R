a <- data.frame(letters=letters, LETTERS=LETTERS, idx = 1:26, 
                stringsAsFactors=FALSE)
evolDF <- evolution(letters ~ v(idx) + LETTERS, data=a, diff.fun=textDiff)
evolDF
str(evolDF)


text1 <- "This is a short sentence."
text2 <- "This is a very short sentence."
text3 <- "This is a very, very short sentence."

texts <- c(text1, text2, text3)
revisionNum <- c(1,2,3)
authors <- c("Larry", "Curly", "Moe")
component <- rep("Body", times=3)

testingData <- data.frame(texts, revisionNum, authors, component, 
                          stringsAsFactors=FALSE)

testingEvol <- evolution(texts ~ v(revisionNum) + authors, data=testingData, diff.fun=textDiff)
str(testingEvol)
testingEvol
