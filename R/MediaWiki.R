#
# MediaWiki.R
#

#' Wrapper function for Wikipedia Special:Export
#' 
#' For repeated use of the Special:Export functionality in the WikiMedia API
#' specifically for Wikipedia, it made sense to have a convenience function.
#' 
#' @param pages a character vector of pages to retrieve
#' @param dir the direction in which the revisions should be sorted
#' @param offset the version with which to start
#' @param limit the number of versions to return
#' 
#' @return the XML content returned
#' 
#' @example
#' \dontrun{
#' bosox <- MediaWikiSpecialExport("Boston_Red_Sox", offset="2015-01-01T04:39:58Z", limit=3)
#' nchar(bosox)
#' }
#' 
#' @export
MediaWikiSpecialExport <- function(pages, dir=c("asc", "desc"), offset=1, 
                                   limit=5) {
  stopifnot(is.character(pages))
  linefeed <- "%0A"
  endpoint <- "http://en.wikipedia.org/w/index.php"
  title <- "Special:Export"
  pages <- paste(pages, collapse=linefeed)
  body <- list(title=title, pages=pages, offset=offset, limit=limit, 
               action="submit")
  r <- httr::POST(endpoint, body=body, encode="form")
  content(r, "text")
}

xmlTreeParse(bosox, asText=TRUE)

#' Data extraction class
#' 
#' Given the standard format of the Special:Export API function, this object
#' can parse the results and produce a \code{data.frame}. This class is INCOMPLETE, 
#' and does not return the right text yet.
#' 
#' This class was adapted from my answers to an earlier assignment in the 
#' course.
#' 
#' @example
#' \dontrun{
#' parser <- MediaWikiSpecialExportParser$new()
#' handler <- xmlEventParse(file=bosox, branches=parser$saxHandler(), asText=TRUE)
#' revisions <- parser$getDetails()
#' str(revisions)
#' }
#' 
#' @export
MediaWikiSpecialExportParser <- R6Class("MediaWikiSpecialExportParser",
  public=list(
    initialize = function() {
    },
    saxHandler = function() {
       page <- function(context, node, attrs, ...) {
         title <- xmlValue(xmlElementsByTagName(node, name="title")[[1]])
         pageId <- xmlValue(xmlElementsByTagName(node, name="id")[[1]])
         revisionNodes <- xmlElementsByTagName(node, name="revision")
         for (revNode in revisionNodes) {
           timestamp <- xmlValue(xmlElementsByTagName(revNode, name="timestamp")[[1]])
           contributorNodes <- xmlElementsByTagName(revNode,name="contributor")
           username <- NA
           userId <- NA
           if (length(contributorNodes) > 0) {
             usernameNodes <- xmlElementsByTagName(
               contributorNodes[[1]], name="username")
             if (length(usernameNodes) > 0) {
               username <- xmlValue(usernameNodes[[1]])
             }
             userIdNodes <- xmlElementsByTagName(
               contributorNodes[[1]], name="id")
             if (length(userIdNodes) > 0) {
               userId <- xmlValue(userIdNodes[[1]])
             }
           }
           text <- xmlValue(xmlElementsByTagName(revNode, name="text")[[1]])
           dfRow <- list(title=title,
                         pageId=pageId,
                         timestamp=timestamp, 
                         contributorId=userId, 
                         contributorName=username,
                         text=text)
           private$addRevision(dfRow)
         }
       }
       c(page=xmlParserContextFunction(page))
     },
     getDetails = function() {
       private$details
     }
   ),
   private=list(
     details = NULL,
     addRevision = function(dfRow) {
       if (is.null(private$details)) {
         private$details <- data.frame(dfRow, stringsAsFactors=FALSE)
       } else {
         private$details <- rbind(private$details, dfRow)
       }
     }
   )
)