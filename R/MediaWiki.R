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
#' bosox <- MediaWikiSpecialExport("Boston_Red_Sox", offset="2015-01-01T04:39:58Z", limit=1)
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


## Function to extract the results
# title
# revisionid
# revisionTime
# contributorName
# contributorId
# text