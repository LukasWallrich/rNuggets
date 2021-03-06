% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{simplify_factor-deprecated}
\alias{simplify_factor-deprecated}
\title{Collapse factor levels into "Other"}
\usage{
simplify_factor(large_factor, cats, other)
}
\arguments{
\item{large_factor}{The existing factor}

\item{cats}{The levels to keep}

\item{other}{The name of the new "other"-level}
}
\description{
This function duplicates [forcats::fct_other()]. It is therefore
deprecated and should not be used. At present, I need it for compatibility.
It will be removed in the future.
}
\seealso{
\code{\link{rNuggets-deprecated}}
}
\keyword{internal}
