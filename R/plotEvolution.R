## 
## plotEvolution.R
## 

## plot method for evolution objects
## TODO restructure this to get data from the REAL evolution object...

plot.evolution <- function(
  evolObj, startAtTop = TRUE, interactive = TRUE, age = TRUE, 
  colorPal = c("topo", "rainbow", "warm"),
  xtitle = NULL) {
  
  stopifnot(inherits(evolObj, "evolution"))
  stopifnot(is.logical(startAtTop), is.logical(interactive), is.logical(age))
  colorPal <- match.arg(colorPal)
  
  edf <- .buildRenderDf(evo)
  yclassUq <- .getUniqueClasses(edf)
  colorMx <- .buildColorMatrix(yclassUq, colorPal)
  edf <- .populateRenderDf(edf, startAtTop)
  evolV <- .renderEvol(edf, colorMx, yclassUq, xtitle, interactive, age)
  evolV
}

## Build internal DF w cols from given object
.buildRenderDf <- function(evolObj) {
  stopifnot(inherits(evolObj, "evolution"))
  df <- attr(evo,"data")
  if (is.null(df)) stop("'data' attribute missing, expecting data frame.")
  # Build from terms (?) and validate...
  
  df <- .setYvolumes(df)
  df
}

## Set DF yvol based on ycontent col
.setYvolumes <- function(edf) {
  if (is.numeric(edf[,"ycontent"])) {
    edf[,"yvol"] <- edf[,"ycontent"]
  } else {
    edf[,"yvol"] <- nchar(edf[,"ycontent"])
  }
  edf
}

## Get unique classes in yclass column
.getUniqueClasses <- function(edf) {
  yclassUq <- unique(edf[,"yclass"])
  yclassUq
}

## Build color matrix
.buildColorMatrix <- function(classVec, colorRange) {
  stopifnot(is.character(classVec))
  if (colorRange != "topo") stop("Only 'topo' is currently supported.")
  ## Currently four shades for aging color
  colvec1 <- .buildColorVec(classVec, 1.00)
  colvec2 <- .buildColorVec(classVec, 0.85)
  colvec3 <- .buildColorVec(classVec, 0.70)
  colvec4 <- .buildColorVec(classVec, 0.55)
  colmx <- matrix(data=c(colvec1, colvec2, colvec3, colvec4), ncol=4)
  colmx
}

## Build color vector
.buildColorVec <- function(classVec, hsvValue) {
  ## start and end values set the 'topo' hue range
  stopifnot(is.character(classVec), is.numeric(hsvValue))
  if (hsvValue < 0.0 || hsvValue > 1.0) stop("hsvValue must be in range 0 to 1.")
  rainbow(length(classVec), 
          alpha=0.7, 
          s=1, 
          start=.15, end=.55, 
          v=hsvValue)
}

## Populate columns for rendering
.populateRenderDf <- function(edf, startAtTop) {
  xvalmin <- min(edf[,"xval"])
  currxval <- -1
  for (i in 1:length(edf[,1])) {
    # Reset when new xval found
    if (edf[i,"xval"] != currxval) {
      currxval <- edf[i,"xval"]
      yvolmax <- 0
    }
    # When past xvalmin, look for earlier matches of class+comp
    ldf <- edf[edf$yclass==edf[i,"yclass"] & edf$ycomp==edf[i,"ycomp"] & edf$xval < currxval,]
    # If earlier match found, get latest one (max xval)
    if (length(ldf[,"xval"]) > 0) {
      xvalmax <- max(ldf[,"xval"])
      # ldf is row with previous xval of this class+comp
      ldf <- ldf[ldf$xval == xvalmax,]
      edf[i,"y1"] <- yvolmax
      edf[i,"y2"] <- edf[i,"y1"] + edf[i,"yvol"]
      edf[i,"yage"] <- min(ldf[1,"yage"] + 1, 4)
      yvolmax <- edf[i,"y2"]
    } else {
      # Otherwise start new class+comp
      edf[i,"y1"] <- yvolmax
      edf[i,"y2"] <- edf[i,"y1"] + edf[i,"yvol"]
      edf[i,"yage"] <- 0
      yvolmax <- edf[i,"y2"]
    }
  }
  
  ## Add max xval rows so rightmost x interval will be visible
  xvalmax <- max(edf[,"xval"])
  xmaxrows <- edf[edf$xval == xvalmax,]
  xmaxrows[,"xval"] <- xvalmax + 1
  xmaxrows[,"yage"] <- pmin(xmaxrows[,"yage"] + 1, rep(4, dim(xmaxrows)[1]))
  edf <- rbind(edf, xmaxrows)
  
  ## Transform y1 and y2 vals if visualizing y axis from top-down
  if (startAtTop) {
    edf[,"y1"] <- max(edf[,"y2"]) - edf[,"y1"]
    edf[,"y2"] <- max(edf[,"y2"]) - edf[,"y2"]
  }
  edf
}

## Render the object
.renderEvol <- function(edf, colorMx, yclassUq, xtitle, interactive, age) {
  
  if ( !require("ggvis", quietly=TRUE)) stop("Rendering requires package ggvis.")
  vis <- ggvis::ggvis(
    data = edf, x = ~xval, y = ~y1, y2 = ~y2,
    stroke := "gray", strokeWidth := 1) 
  
  xvalmin <- min(edf[,"xval"])
  currxval <- -1
  for (i in 1:length(edf[,1])) {
    # reset to zero when new xval found
    if (edf[i,"xval"] != currxval) {
      currxval <- edf[i,"xval"]
    }
    if (edf[i,"xval"] > xvalmin) {
      # Past minimum xval, look for earlier matches of class+comp
      ldf <- edf[edf$yclass == edf[i,"yclass"] & 
                   edf$ycomp == edf[i,"ycomp"] & 
                   edf$xval < currxval,]
      # If earlier match found, get latest one (max xval)
      if (length(ldf[,"xval"]) > 0) {
        xvalmax <- max(ldf[,"xval"])
        # ldf is row with previous xval of this class+comp
        ldf <- ldf[ldf$xval == xvalmax,]
        # Together, the previous row and current row 
        # are the current ggvis data object
        ldf <- rbind(ldf, edf[i,])
        # Color indexing      
        colrow <- base::match(edf[i,"yclass"], yclassUq)
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
    vis <- ggvis::add_axis(vis, type = "x", title = "Evolution")
  } else {
    vis <- ggvis::add_axis(vis, type = "x", title = xtitle)
  }
  vis <- ggvis::add_axis(vis, type = "y", title = "")
  
  # Tooltips show class, component and header of content
  # TODO trim and format alpha content to show header + "..." if too long
  renderTips <- function(x) {
    if(is.null(x)) return(NULL)
    # For POSIXct GMT date xval:
    if ("POSIXct" %in% class(edf[1,"xval"])) {
      xvalPOSIXct <- as.POSIXct(x$xval/1000, origin="1970-01-01", tz="GMT")
      row <- edf[xvalPOSIXct==edf[,"xval"] & x$y1==edf[,"y1"],]
    }
    # For numeric xval:
    else {
      row <- edf[x$xval==edf[,"xval"] & x$y1==edf[,"y1"],]
    }
    # Data not found:
    if (dim(row)[1] < 1) return(NULL)
    paste(c(row$yclass, row$ycomp, row$ycontent), collapse = "<br />")
  }
  # Add tooltips if interactive requested
  if (interactive) {
    vis <- ggvis::add_tooltip(vis, renderTips, "hover")
  }
  vis
}
