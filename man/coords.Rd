% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shade.R
\name{coords}
\alias{coords}
\title{Retrieve the coordinates of a colour vector}
\usage{
coords(x, ...)
}
\arguments{
\item{x}{An R object.}

\item{...}{Additional arguments to methods.}
}
\value{
A matrix giving colour coordinates in the relevant space, one colour
  per row. Columns are typically named.
}
\description{
This function retrieves the coordinates of a colour vector's elements,
within whatever space it is currently defined.
}
\examples{
coords("red")
}
\author{
Jon Clayden <code@clayden.org>
}
