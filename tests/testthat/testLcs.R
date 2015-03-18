a <- lcs()
a

b <- lcs("b", 1, 4)
b

combineLcs(a,b)

## Example from wikipedia page
## http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

computeLcs("GAC", "AGCAT")
computeLcs("XMJY .AUZ", "MZJ .AWXU")
x <- "This is a sentence."
y <- "This is a very short sentence."
exampleLCS <- computeLcs(x, y)


# Stan: temp comment out: sequences obj not found: 
# longIndex <- attr(sequences, "y.index")
# longIndex
# y.mod <- unlist(strsplit(y, split=""))
# 1:length(y.mod)
# textDiff(x,y)
# is.integer(attr(sequences, "x.index"))
# is.numeric(attr(sequences, "x.index"))
