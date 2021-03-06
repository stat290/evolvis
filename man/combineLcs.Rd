% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/lcs.R
\name{combineLcs}
\alias{combineLcs}
\title{Concatenate two LCS objects}
\usage{
combineLcs(lcs1, lcs2)
}
\arguments{
\item{lcs1}{the first LCS}

\item{lcs2}{the second LCS}
}
\value{
an LCS object in which the strings, \code{x.index}, and
\code{y.index} have each been concatenated with the corresponding pair.
}
\description{
Given two LCS objects, concatenate them in the order given.
}

