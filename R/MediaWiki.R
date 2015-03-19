#
# begin kjohnsto code
#

#' Wikipedia revision data for the Boston_Red_Sox page
#'
#' An example XML output of Special:Export for the Boston_Red_Sox page.
#' 
#' @format a character vector of length 1
#' @source was the result of a call to MediaWikiSpecialExport:
#' MediaWikiSpecialExport("Boston_Red_Sox", offset="2015-01-01T04:39:58Z", limit=3)
"bosox"

#' Wikipedia revision data for the Small page
#'
#' An example XML output of Special:Export for the Small page.
#' 
#' @format a character vector of length 1
#' @source was the result of a call to MediaWikiSpecialExport:
#' MediaWikiSpecialExport("small", offset="2015-01-01T00:00:00Z", limit=6)
"small"


#' Wrapper function for Wikipedia Special:Export
#' 
#' For repeated use of the Special:Export functionality in the WikiMedia API
#' specifically for Wikipedia, it made sense to have a convenience function.
#' 
#' @param pages a character vector of pages to retrieve
#' @param offset the version with which to start
#' @param limit the number of versions to return
#' 
#' @return the XML content returned
#' 
#' @examples
#' \dontrun{
#' bosox <- MediaWikiSpecialExport("Boston_Red_Sox", offset="2015-01-01T04:39:58Z", limit=3)
#' small <- MediaWikiSpecialExport("small", offset="2015-01-01T00:00:00Z", limit=6)
#' }
#' nchar(bosox)
#' nchar(small)
#' 
#' 
#' @export
MediaWikiSpecialExport <- function(pages, offset=1, limit=5) {
  stopifnot(is.character(pages))
  linefeed <- "%0A"
  endpoint <- "http://en.wikipedia.org/w/index.php"
  title <- "Special:Export"
  pages <- paste(pages, collapse=linefeed)
  body <- list(title=title, pages=pages, offset=offset, limit=limit, 
               action="submit")
  r <- httr::POST(endpoint, body=body, encode="form")
  httr::content(r, "text")
}

#' Data extraction class
#' 
#' Given the standard format of the Special:Export API function, this object
#' can parse the results and produce a \code{data.frame}.
#' 
#' This class was adapted from my answers to an earlier assignment in the 
#' course.
#' 
#' @return a parser object
#' 
#' @examples
#' \dontrun{
#' parser <- MediaWikiSpecialExportParser()
#' handler <- XML::xmlEventParse(file=small, branches=parser$saxHandler(), asText=TRUE)
#' revisions <- parser$details()
#' str(revisions)
#' revisions
#' }
#' 
#' @export
MediaWikiSpecialExportParser <- function() {
  details <- NULL
  addRevision <- function(dfRow) {
    if (is.null(details)) {
      details <<- data.frame(dfRow, stringsAsFactors=FALSE)
    } else {
      details <<- rbind(details, dfRow)
    }
  }
  saxHandler <- function() {
    page <- function(context, node, attrs, ...) {
      title <- XML::xmlValue(XML::xmlElementsByTagName(node, name="title")[[1]])
      pageId <- XML::xmlValue(XML::xmlElementsByTagName(node, name="id")[[1]])
      revisionNodes <- XML::xmlElementsByTagName(node, name="revision")
      for (revNode in revisionNodes) {
        timestamp <- XML::xmlValue(XML::xmlElementsByTagName(revNode, name="timestamp")[[1]])
        contributorNodes <- XML::xmlElementsByTagName(revNode,name="contributor")
        username <- NA
        userId <- NA
        if (length(contributorNodes) > 0) {
          usernameNodes <- XML::xmlElementsByTagName(
            contributorNodes[[1]], name="username")
          if (length(usernameNodes) > 0) {
            username <- XML::xmlValue(usernameNodes[[1]])
          }
          userIdNodes <- XML::xmlElementsByTagName(
            contributorNodes[[1]], name="id")
          if (length(userIdNodes) > 0) {
            userId <- XML::xmlValue(userIdNodes[[1]])
          }
        }
        textNodes <- XML::xmlElementsByTagName(revNode, name="text")
        for (textNode in textNodes) {
          if (!is.null(XML::xmlGetAttr(textNode, "bytes"))) {
            text <- XML::xmlValue(textNode)
          }
        }
        dfRow <- list(title=title,
                      pageId=pageId,
                      timestamp=timestamp, 
                      contributorId=userId, 
                      contributorName=username,
                      text=text)
        addRevision(dfRow)
      }
    }
    c(page=XML::xmlParserContextFunction(page))
  }
  list(details=function() details, saxHandler=saxHandler)
}
#
# end kjohnsto code
#
