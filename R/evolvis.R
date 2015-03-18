#
# evolvis.R 
# A NULL code file for generating roxygen2 package documentation
#
#' \strong{evolvis} is \strong{evol}ution \strong{vis}ualization.
#'
#' The goal of evolvis is a generalized toolset for generating 
#' lineage of some composition, such as a document, from a series 
#' of events such as revisions, and visualizing the lineage interactively.
#' 
#' The evolvis package provides three categories of functions
#' which work together:
#' XML transform
#' Text differencing
#' Interactive visualization
#' 
#' @section XML transform:
#' Transforms XML documents into a series of text revisions.  For the 
#' initial use case these are WikiMedia documents, which is the XML
#' schema for Wikipedia pages.  When connected to the internet, 
#' XML documents can be downloaded on demand.
#'
#' @section Text differencing:
#' The \code{evolution} function is the main entry point.  It uses 
#' formula syntax to express the evolution of differences in the 
#' response variable \code{y} as a function of the version \code{v(...)}
#' by content class, for example \code{texts ~ v(revisionNum) + author}.
#' \code{diff.fun} is required and specifies the differencing function.
#' 
#' @section Interactive visualization:
#' The plot.evolution function is an S3 dispatch method for the
#' internal generic plot function.  It requires the \code{ggvis} 
#' package and produces a \code{ggvis} object which can be rendered 
#' using the generic \code{print} method or \code{ggvis::view_dynamic} 
#' or \code{ggvis::view_static}.
#' 
#' @section Background:
#' The package was inspired by the IBM Research \emph{History Flow} project
#' \url{https://www.research.ibm.com/visual/projects/history_flow/} 
#' for visualizaing the history and composition of Wikipedia articles.
#' 
#' @docType package
#' @name evolvis
NULL
#