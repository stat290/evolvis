x <- "This is a sentence."
y <- "This is a very short sentence."
z <- "This is a very, very short sentence."
exampleLCS <- computeLcs(x, y)
.diffIndexFromLcs(nchar(x), nchar(y), exampleLCS)
.diffFromLcs(x, y, exampleLCS)
d1 <- .singleTextDiff(x, y)
d2 <- .singleTextDiff(y, z)
.mergeTextDiff(d1,d2)
textDiff(c(x,y,z))


d <- .singleTextDiff(x, y)
df <- as.data.frame(d, stringsAsFactors=FALSE)
str(df)
df[1:45,]
