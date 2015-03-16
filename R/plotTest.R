##
## plotTest.R
##

## TODO restructure tests and plot to use a REAL evolution object...

evo <- structure(1, class = "evolution")

attr(evo, "data") <- getEvolTestDf1()
vis <- plot(evo) ## method dispatch
## This is a plain old ggvis object, 
## so you can keep building on it if you like
vis

attr(evo, "data") <- getEvolTestDf2()
plot(evo, startAtTop = FALSE)

attr(evo, "data") <- getEvolTestDf3()
plot(evo, xtitle = "The Legend of St Patty")


## ==== Random chronology of a novel authored by six of The Dwarves ===
getEvolTestDf1 <- function() {
  n <- 9 ## Chapters
  xval <- unlist(sapply(1:n, function(x) rep(x,x)))
  sixParts <- c("Dopey", "Sleepy", "Doc", "Happy", "Bashful", "Grumpy")
  sixIndex <- 0:(n-1) %% 6 + 1
  yclass <- paste("Author:", unlist(sapply(1:n, function(x) sixParts[sixIndex[1:x]])))
  ycomp <- paste("Chapter", unlist(sapply(1:n, function(x) seq(x))))
  ycontent <- round(runif(length(xval), min=1, max=100), digits=0)
  edf1 <- data.frame(xval, yclass, ycomp, ycontent, stringsAsFactors = FALSE)
  edf1
}
## ====================================================================

## ==== Investment portfolio with shifting volumes and DATES ==========
## 

getEvolTestDf2 <- function() {
  xval <- rep(ISOdatetime(2008:2015,1,1,12,0,0,tz="GMT"),4)
  xval <- xval[order(xval)]
  fourParts <- c("Equities", "Bonds", "Real Estate", "Commodities")
  yclass <- fourParts[c(1:4,1:4,1:4,2,1,3,4,2,1,4,3,2,1,4,3,2,1,4,3,1,2,4,3)]
  ycomp <- rep("",32)
  ycontent <- c(4,3,2,1,6,5,3,2,5,4,2,1,5,4,3,2,6,5,3,1,7,5,4,2,8,6,3,2,8,7,4,3)
  edf1 <- data.frame(xval, yclass, ycomp, ycontent, stringsAsFactors = FALSE)
}
## ====================================================================

## ==== Toy data frame that illustrates changing text content =========
## The authors and text can be seen by mouseover on the graph
## if interactive=TRUE and rendered in a browser

getEvolTestDf3 <- function() {
  xval <-   c(1,      2,       2,       2,     3,       3,       
              3,     3,      4,      4,      4,     4,     4)
  yclass <- c("Fred", "Fred",  "Sue",   "Sue", "Fred",  "Sue",   
              "Sue", "Fred", "Fred", "Sue",  "Sam", "Sue", "Fred")
  ycomp <-  c("Intro","Intro", "Intro", "Body","Intro", "Intro", 
              "Body","Hist", "Intro","Intro","Ref", "Body","Hist")
  ycontent<-c("There was once a leprechaun. ",
              "There was once a ",
              "saint. ",
              "He saved Ireland. ",
              "There was once a ",
              "saint. ",
              "He saved Ireland and invented green beer. ",    
              "The rest is history.",     
              "There was once a ",
              "saint. ",
              "See the Book of Celts. ",    
              "He saved Ireland and invented green beer. ",    
              "The rest is history.")
  edf1 <- data.frame(xval, yclass, ycomp, ycontent, stringsAsFactors = FALSE)
}
## =====================================================================
