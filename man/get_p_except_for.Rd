% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-generics.R, R/get_p_except_for.R
\name{get_p_except_for}
\alias{get_p_except_for}
\alias{get_p_except_for.macroarea}
\alias{get_p_except_for.default}
\title{Get the probability of acceptance exept for a given center}
\usage{
get_p_except_for(macroarea, offering_region)

\method{get_p_except_for}{macroarea}(macroarea, offering_region)

\method{get_p_except_for}{default}(macroarea, offering_region)
}
\arguments{
\item{macroarea}{an object used to select a method.}

\item{offering_region}{a region name into the area}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The probability of acceptance for the \code{macroarea} don't
        considering the \code{offering_region}.
}
\description{
This is a generic function aimed to access (or compute) to the
probability of acceptance (for an offered organ) by an area
excluding the (macro-)region how have offered the organ.
}
\section{Methods (by class)}{
\itemize{
\item \code{macroarea}: method to access to the detail
"initial_strip".

\item \code{default}: method to access to the detail
"initial_strip".
}}

