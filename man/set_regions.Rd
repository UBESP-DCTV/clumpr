% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macroregion.R
\name{set_regions}
\alias{set_regions}
\alias{print.set_regions}
\title{List of regions creator}
\usage{
set_regions(...)

\method{print}{set_regions}(x, ...)
}
\arguments{
\item{...}{a set of \code{\link{region}}s}

\item{x}{an object used to select a method.}
}
\value{
The list of \code{\link{region}}s provided.
}
\description{
Function used to prepare and check list of regions to pass to
\code{regions} as needed to create \code{\link{macroregion}} objects.
}
\section{Methods (by generic)}{
\itemize{
\item \code{print}: nice (and coloured, if supported) print method.
}}

\examples{
pavia  <- center('Pavia',  'Lombardia')
milano <- center('Milano', 'Lombardia')
lombardia <- region(set_centers(pavia, milano))

padova <- center('Padova', 'Veneto', 8, 0.7)
veneto <- region(set_centers(padova))

set_regions(lombardia, veneto)
}
