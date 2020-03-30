#' Tunnel a dataframe through a function for side effects
#'
#' Within a dplyr-pipe, you might want to print a summary statistic or run some
#' other function before continuing to process or save the original data. This
#' function does that by taking a dataframe and function as arguments, printing
#' the result of the function and returning the dataframe for further processing.
#'
#' @param df A dataframe
#' @param fun A function that is applied to the dataframe and the
#' output of which is printed
#' @param ... Arguments passed on to \code{fun}
#' @param note A string that is printed above the \code{fun}-output
#' @param return Logical - should df be returned?
#' @return The original dataframe, unless \code{return} is set to FALSE. In that
#'   case, NULL is returned invisibly, so that the function can be used to just
#'   add a note above the output of another function.
#' @source Inspired by the magrittr \code{\%T>\%} operator that promises similar
#'   functionality but often didn't quite fit my needs
#' @examples
#' library(magrittr)
#' x <- 1:5 %>% tunnel(mean, note="Mean") %>% tunnel(sd, note="SD")
#' x

tunnel <- function(df, fun, ..., note=NULL, return=T) {
  if (!is.null(note)) print(note)
  print(fun(df, ...))
  if (return) return(df) else invisible(NULL)
}

#' lm() for pipes - data as first argument
#'
#' Within a dplyr-pipe, running lm() is often complicated be the placing of the
#' data argument. This wrapper places data first.
#'
#' Note that the model formula is replaced by a string in the model object -
#' this is a quick fix for a bug that led to the fomula not being displayed
#' but could potentially lead to downstream issues.
#'
#' @param df Data for modeling
#' @inheritParams stats::lm
#' @inheritDotParams stats::lm

run_lm <- function(df, formula, ...) {
  x<-stats::lm(data=df, formula, ...)
  x$call[2] <- deparse(formula)
  x
}

