% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gips_perm_class.R
\name{print.gips_perm}
\alias{print.gips_perm}
\title{Printing \code{gips_perm} object}
\usage{
\method{print}{gips_perm}(x, ...)
}
\arguments{
\item{x}{An object of a \code{gips_perm} class.}

\item{...}{Further arguments (currently ignored).}
}
\value{
Returns an invisible \code{NULL}.
}
\description{
Printing function for a \code{gips_perm} class.
}
\examples{
gperm <- gips_perm("(5,4)", 5)
print(gperm)
}
