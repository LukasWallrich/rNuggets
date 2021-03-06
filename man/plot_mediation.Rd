% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/med_mod_plots.R
\encoding{UTF-8}
\name{plot_mediation}
\alias{plot_mediation}
\title{Plot mediation model with one or more mediators}
\usage{
plot_mediation(
  IV,
  DV,
  Ms,
  df,
  digits = 2,
  coef_offset = length(Ms),
  filename = NULL,
  ind_p_values = FALSE
)
}
\arguments{
\item{IV}{Character. Name of predictor}

\item{DV}{Character. Name of dependent variable}

\item{Ms}{Character vector. Names of mediator variables}

\item{df}{Dataframe with coefficients and significance values. See details.}

\item{digits}{Number of digits for rounding}

\item{coef_offset}{Tibble with values to position mediators. If not
provided, function will align mediators automatically, which is unlikely to
provide a well-aligned path (except for cases when offset has been implemented
for that number of mediators, currently 1 and 3). However, returned code can still
be edited. See rNuggets:::.coef_offset_3 for an example of an offset tibble.}

\item{filename}{If provided, graph will be saved as .svg file.}

\item{ind_p_values}{Should significance stars be shown for indirect effects,
based on pvalues passed in DF? If FALSE, indirect effects with confidence
intervals that do not include zero are bolded}
}
\value{
A list of a the graph and the associated code.
}
\description{
Returns graphViz code and graph for (multiple) mediation model.
Coefficients and significance  values for paths need to be provided -
see example for the format.
}
\examples{
\dontrun{
# Values for model
med_model <- tibble::tribble(
  ~type, ~mediator, ~est, ~pvalue, ~ci.lower, ~ci.upper,
  "a", "Empathy", 0.29, 0, 0.29, 0.29,
  "b", "Empathy", 0.18, 0, 0.18, 0.18,
  "direct", NA, -0.08, 0, -0.08, -0.08,
  "indirect", "Empathy", 0.05, 0, 0.05, 0.05,
  "total", NA, 0.41, 0, 0.41, 0.41
)

# Run plot command
rNuggets:::plot_mediation(
  IV = "Positive <br /> contact",
  DV = "Diversity <br /> policies", Ms = "Empathy", df = med_model
)
}
}
