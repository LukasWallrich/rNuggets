% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_desc_table.R
\name{cat_var_table_mi}
\alias{cat_var_table_mi}
\title{Create a summary table for categorical variables and their relationship with continuous DV for multiply imputed data}
\usage{
cat_var_table_mi(
  mi_list,
  dv,
  weights,
  ...,
  var_names = NULL,
  level_names = NULL,
  p.adjust = p.adjust.methods,
  alpha_level = 0.05,
  filename = NULL,
  notes = list(),
  dv_name = NULL,
  bold_vars = TRUE,
  css_tags = list()
)
}
\arguments{
\item{mi_list}{A list of dataframes containing multiple imputations of a dataset}

\item{dv}{The continuous dependent variable to be presented alongside the levels of categorical variables}

\item{weights}{Survey weights to be used. Currently required for the function to work, create and pass a column of 1s in mi_list if weights should be uniform.}

\item{...}{Categorical variables to be included}

\item{var_names}{Tibble of old and new variable names, if variables are to be renamed for display. See \code{\link{get_rename_tribbles}} for required format}

\item{level_names}{Tibble of old and new level names, if levels are to be renamed for display. See \code{\link{get_rename_tribbles}} for required format}

\item{p.adjust}{One of p.adjust.methods, defaults to Holm}

\item{alpha_level}{The level of significance for the pairwise comparisons (after p.adjust). Defaults to .05}

\item{filename}{the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)}

\item{notes}{List of notes to append to bottom of table.}

\item{dv_name}{Optional. A different name to use for the dependent variable in the automatic table footnote explaining the M(SD) column. Defaults to dv variable name.}

\item{bold_vars}{Should rows with variable names be bold. Defaults to TRUE}

\item{css_tags}{List of css tags to be added, each named with the class that the tag should be added to.}
}
\value{
A list including a tibble of descriptive statistics (`descr`), the `gt`-table (`tab`) and the HTML code (`html`) with `css_tags` added
}
\description{
This takes a set of categorical variables and a continuous dependent variable and
produces a table that shows the distribution of data across the levels of the categorical
variables, as well as the mean of the dependent variable for each level and the significance
of pairwise comparisons between these means.
}
\details{
To create a list suitable for `mi_list` from a mids object, run `mice::complete(mids, action="long", include = FALSE)``
}
