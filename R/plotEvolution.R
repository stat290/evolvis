# 
# plotEvolution.R
# 


#' Create an evolution visualization
#' 
#' It is the evolution dispatch method for the internal generic \code{plot}
#'
#' \code{plot(evolutionObj, ...} or \code{plot.evolution(...)} produces a
#' visualization object of type \code{ggvis}.  It can be passed on to 
#' other operations in the \code{ggvis} package, such as 
#' \code{ggvis::view_dynamic(...)} and  \code{ggvis::add_legend(...)}.
#' 
#' Use code like \code{ggvis::view_dynamic(vis, port = 3838)} to visualize
#' interactivity in external local browser at \code{http://localhost:3838/}
#' 
#' @param x The object of type \code{evolution}.
#' @param startAtTop A logical scalar. Render starting at top of y axis?
#' This may be preferred for document visualization, so that visually it
#' grows \emph{from the top down}.
#' @param interactive A logical scalar. Render interactive features?
#' When TRUE, tooltips showing the class of content are shown on mouseover.
#' When FALSE a static image will be rendered.
#' @param age A logical scalar. When TRUE, colors are \emph{aged} over
#' time, by getting darker as they progess along the x axis.
#' @param colorPal A character scalar, either \code{topo}, \code{rainbow},
#' or \code{warm} which specifies the range of hues in the HSV color model.
#' @param xtitle A character scalar.  Title shown on the x axis.  If not
#' supplied, a title will be created using the evolution formula terms.
#' @param ... Additional parameters to ggvis.
#'
#' @return Returns a \code{ggvis} visualization object.
#' 
#' @examples
#' \dontrun{
#' evo <- evolution(texts ~ v(rev) + author, data=data, diff.fun=textDiff)
#' vis <- plot(evo)
#' view_dynamic(vis, port = 3838)
#' }
#'
#' @export
plot.evolution <- function(
  x, ..., startAtTop = TRUE, interactive = TRUE, age = TRUE, 
  colorPal = c("topo", "rainbow", "warm"),
  xtitle = NULL) {
  
  stopifnot(inherits(x, "evolution"))
  stopifnot(is.logical(startAtTop), is.logical(interactive), is.logical(age))
  colorPal <- match.arg(colorPal)
  
  grpTerms <- .getGroupTerms(x)
  edf <- .buildRenderDf(x)
  yclassUq <- .getUniqueClasses(edf, grpTerms)
  colorMx <- .buildColorMatrix(yclassUq, colorPal)
  edf <- .populateRenderDf(edf, grpTerms, startAtTop)

  evolV <- .renderEvol(edf, xtitle, interactive, age, colorMx, yclassUq, grpTerms)
  evolV
}

# Get char vector of terms to use for grouping (exclude special terms)
.getGroupTerms <- function(evolObj) {
  stopifnot(inherits(evolObj, "evolution"), inherits(evolObj, "data.frame"))
  if (is.null(evolObj)) stop("Invalid NULL evolution object.")
  
  # TODO make this more general - support multiple columns and
  # exclude specials using: names(attr(attr(evolObj, "terms"), "specials"))
  
  # Simple implementation: get term.labels except first term (assume v(..))
  groupTerms <- attr(attr(evolObj, "terms"), "term.labels")[-1]
  groupTerms
}

# Build internal DF w cols from given object
.buildRenderDf <- function(evolObj) {
  evolObj <- .setYvolumes(evolObj)
  evolObj
}

# Set DF yvol based on value col
.setYvolumes <- function(edf) {
  if (is.numeric(edf[,"value"])) {
    edf[,"yvol"] <- edf[,"value"]
  } else {
    edf[,"yvol"] <- nchar(edf[,"value"])
  }
  edf
}

# Get unique classes - class is the combination of group terms
.getUniqueClasses <- function(edf, groupTerms) {
  groupCol <- .getGroupColumns(edf, groupTerms)
  yclassUq <- unique(edf[,groupCol])
  yclassUq
}

# Map column names to column numbers in the data frame
.getGroupColumns <- function(edf, groupTerms) {
  if (length(groupTerms) > 1) stop("multi-term groups not yet supported")
  groupCol <- match(groupTerms, colnames(edf))
  groupCol
}

# Build color matrix
.buildColorMatrix <- function(classVec, colorRange) {
  stopifnot(is.character(classVec))
  if (colorRange != "topo") stop("Only 'topo' is currently supported.")
  # Currently four shades for aging color
  colvec1 <- .buildColorVec(classVec, 1.00)
  colvec2 <- .buildColorVec(classVec, 0.85)
  colvec3 <- .buildColorVec(classVec, 0.70)
  colvec4 <- .buildColorVec(classVec, 0.55)
  colmx <- matrix(data=c(colvec1, colvec2, colvec3, colvec4), ncol=4)
  colmx
}

# Build color vector
.buildColorVec <- function(classVec, hsvValue) {
  # start and end values set the 'topo' hue range
  stopifnot(is.character(classVec), is.numeric(hsvValue))
  if (hsvValue < 0.0 || hsvValue > 1.0) 
    stop("hsvValue must be in range 0 to 1.")
  rainbow(length(classVec), 
          alpha=0.7, 
          s=1, 
          start=.15, end=.55, 
          v=hsvValue)
}

# Populate columns for rendering
.populateRenderDf <- function(edf, groupTerms, startAtTop) {
  
  groupCol <- .getGroupColumns(edf, groupTerms)
  xvalmin <- min(edf[,"version"])
  currxval <- -1
  for (i in 1:length(edf[,1])) {
    # Reset when new xval found
    if (edf[i,"version"] != currxval) {
      currxval <- edf[i,"version"]
      yvolmax <- 0
    }
    # Look for earlier matches 
    ldf <- edf[edf[,groupCol]==edf[i,groupCol] & 
                 edf$element==edf[i,"element"] & 
                 edf$version < currxval,]
    # If earlier match(es) found, get latest one (max xval)
    if (length(ldf[,"version"]) > 0) {
      xvalmax <- max(ldf[,"version"])
      # ldf is row with previous version
      ldf <- ldf[ldf$version == xvalmax,]
      edf[i,"y1"] <- yvolmax
      edf[i,"y2"] <- edf[i,"y1"] + edf[i,"yvol"]
      edf[i,"yage"] <- min(ldf[1,"yage"] + 1, 4)
      yvolmax <- edf[i,"y2"]
    } else {
      # Otherwise start new 
      edf[i,"y1"] <- yvolmax
      edf[i,"y2"] <- edf[i,"y1"] + edf[i,"yvol"]
      edf[i,"yage"] <- 0
      yvolmax <- edf[i,"y2"]
    }
  }
  
  # Add max xval rows so rightmost x interval will be visible
  xvalmax <- max(edf[,"version"])
  xmaxrows <- edf[edf$version == xvalmax,]
  xmaxrows[,"version"] <- xvalmax + 1
  xmaxrows[,"yage"] <- pmin(xmaxrows[,"yage"] + 1, rep(4, dim(xmaxrows)[1]))
  edf <- rbind(edf, xmaxrows)
  
  # Transform y1 and y2 vals if visualizing y axis from top-down
  if (startAtTop) {
    edf[,"y1"] <- max(edf[,"y2"]) - edf[,"y1"]
    edf[,"y2"] <- max(edf[,"y2"]) - edf[,"y2"]
  }
  edf
}

# Render the ggvis object
.renderEvol <- function(
  edf, xtitle, interactive, age, colorMx, yclassUq, groupTerms) {

  groupCol <- .getGroupColumns(edf, groupTerms)
  # ggvis required ver is in DESCRIPTION file
  # if ( !require("ggvis", quietly=TRUE)) stop()
  vis <- ggvis::ggvis(
    data = as.data.frame(edf), 
    x = ~version, y = ~y1, y2 = ~y2,
    stroke := "gray", strokeWidth := 1)
  # Leftmost x value
  xvalmin <- min(edf[,"version"])
  currxval <- -1
  for (i in 1:length(edf[,1])) {
    # Reset when new xval found
    if (edf[i,"version"] != currxval) {
      currxval <- edf[i,"version"]
    }
    if (edf[i,"version"] > xvalmin) {
      # Past minimum xval, look for earlier matches
      ldf <- edf[edf[,groupCol]==edf[i,groupCol] & 
                   edf$element == edf[i,"element"] & 
                   edf$version < currxval,]
      # If earlier match found, get latest one (max xval)
      if (length(ldf[,"version"]) > 0) {
        xvalmax <- max(ldf[,"version"])
        # ldf is row with previous xval
        ldf <- ldf[ldf$version == xvalmax,]
        # Together, the previous row and current row 
        # are the data obj for the current ggvis::mark
        ldf <- rbind(ldf, edf[i,])
        # Color indexing      
        colrow <- base::match(edf[i,groupCol], yclassUq)
        if (age) {
          colage <- edf[i,"yage"]
        } else {
          colage <- 1
        }
        vis <- ggvis::layer_ribbons(
          vis, 
          fill := substr(colorMx[colrow, colage],1,7), 
          opacity := .7, 
          data = ldf)
      }
    }
  }
  
  # X and Y labels
  if (is.null(xtitle)) {
    vis <- ggvis::add_axis(vis, type = "x", 
                           title = paste(groupTerms, "evolution"))
  } else {
    vis <- ggvis::add_axis(vis, type = "x", title = xtitle)
  }
  vis <- ggvis::add_axis(vis, type = "y", title = "")
  
  # Tooltips show class, component and header of content
  # TODO trim and format alpha content to show header + "..." if too long
  renderTips <- function(x) {
    if(is.null(x)) return(NULL)
    # For POSIXct GMT date xval:
    if ("POSIXct" %in% class(edf[1,"version"])) {
      xvalPOSIXct <- as.POSIXct(x$version/1000, origin="1970-01-01", tz="GMT")
      row <- edf[xvalPOSIXct==edf[,"version"] & x$y1==edf[,"y1"],]
    }
    # For numeric xval:
    else {
      row <- edf[x$version==edf[,"version"] & x$y1==edf[,"y1"],]
    }
    # Data not found:
    if (dim(row)[1] < 1) return(NULL)
    paste(c(row[groupCol], row$ycomp, row$value), collapse = "<br />")
  }
  # Add tooltips if interactive requested
  if (interactive) {
    vis <- ggvis::add_tooltip(vis, renderTips, "hover")
  }
  vis
}
