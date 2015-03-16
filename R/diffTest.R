x <- "This is a sentence."
y <- "This is a very short sentence."
z <- "This is a very, very short sentence."
exampleLCS <- computeLcs(x, y)
.diffIndexFromLcs(nchar(x), nchar(y), exampleLCS)
.diffFromLcs(x, y, exampleLCS)
d1 <- .singleTextDiff(x, y)
d2 <- .singleTextDiff(y, z)
.mergeTextDiff(d1,d2)
diffObj <- textDiff(c(x,y,z))
str(diffObj)
diffObj

texts <- attr(diffObj, "text")
paste(texts[unlist(diffObj[1,])], collapse="") == x
paste(texts[unlist(diffObj[2,])], collapse="") == y
paste(texts[unlist(diffObj[3,])], collapse="") == z

