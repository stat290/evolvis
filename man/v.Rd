% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/evolution.R
\name{v}
\alias{v}
\title{Version function}
\usage{
v(x)
}
\arguments{
\item{x}{a value to check for consistency with a version--"orderable"}
}
\value{
\code{x} if the checks are passed
}
\description{
A simple version function that operates as a flag for \code{evolution} to
sort the data. As differences are order-dependent, \code{evolution} treats
this as a special function in the formula and uses the results as the
natural ordering of the values.
}

