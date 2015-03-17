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
#' These functions ...
#'
#' @section Text differencing:
#' These functions ...
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