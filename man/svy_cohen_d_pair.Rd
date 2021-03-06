% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_tests.R
\name{svy_cohen_d_pair}
\alias{svy_cohen_d_pair}
\title{t.test for survey object with Cohen's d}
\usage{
svy_cohen_d_pair(df, dv, iv, pair, ttest = T)
}
\arguments{
\item{df}{A dataframe}

\item{dv}{Character. Name of the dependent variable for the t.test (numeric)}

\item{iv}{Character. Name of the grouping variable for the t.test (factor)}

\item{pair}{Character vector of length 2. Levels of iv to
be compared in t.test}

\item{ttest}{Logical. Should t.test be run and displayed? Otherwise, only
Cohen's d is calculated. Defaults to TRUE.}
}
\value{
Invisibly returns a list including the t.test() output and
  Cohen's d
}
\description{
This function calculates a t.test() for two groups in a \code{srvyr} survey
object. It is particularly helpful when the grouping variable has more than
two levels and you just want to compare two of them.
}
