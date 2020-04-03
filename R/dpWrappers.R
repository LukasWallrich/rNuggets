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
#' Note that the model call in the lm-object is replaced by the call to this
#' function - that means that \code{update()} cannot be used.
#'
#' @param df Data for modeling
#' @param std Logical. Should variables be standardised? This is only applied to
#' numeric variables, factors are left unchanged so that their coefficients
#' remain interpretable.
#' @param rename_std Logical. Should standardised variables be indicated by _sd
#' suffix
#' @inheritParams stats::lm
#' @inheritDotParams stats::lm -data
#' @source After experiencing an issue with passing weights, I rewrote this
#' based on the code suggested by "Vandenman" here
#' https://stackoverflow.com/questions/38683076/ellipsis-trouble-passing-to-lm
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. If you want to run a model with all variables standardised,
#' one option is `QuantPsyc::lm.beta()`

run_lm <- function(df, formula, std = FALSE, rename_std = FALSE, ...) {

  if (std) {
    vars <- all.vars(formula)
    vars_num <- vars[purrr::map_lgl(vars, .is.numeric_col, df=df)]

    if (rename_std) {
      df <- df %>% dplyr::mutate_at(vars_num, list(sd = scale_blank))


    repl <- paste0(vars_num, "_sd")
    names(repl) <- vars_num
    formula <- Reduce(paste, deparse(formula)) %>%
      stringr::str_replace_all(c(repl)) %>%
      as.formula()
    } else {
      df <- df %>% dplyr::mutate_at(vars_num, list(scale_blank))    }
  }

  # get names of stuff in ...
  arg_names <- sapply(substitute(list(...))[-1L], deparse)
  # look for identical names in df
  m <- match(names(df), arg_names, 0L)

  # store other arguments from ... in a list, if any
  dot_args <- eval(parse(text = arg_names[-m]))
  if (is.null(dot_args)) {
    args <- list()
  } else {
    args <- list(dot_args)
    # name the list
    names(args) <- names(arg_names[-m])
  }

  # store complete values in args, instead of just references to columns
  # the unlist code is rather ugly, the goal is to create a list where every
  # element is a column of interest
  args[names(arg_names)[m]] <- unlist(apply(
    df[, as.logical(m), drop = FALSE],
    2, list
  ), recursive = FALSE)
  # also put other stuff in there
  args$formula <- formula
  args$data <- df
  # do lm
  mod <- do.call(lm, args)
  if (std) {
  mod$call <- c(sys.call(), "Note: DV and continuous IVs were standardised")
  } else {
    mod$call <- c(sys.call())
  }
  mod
}

#' Tests whether a column in df, specificied by string, is numeric

.is.numeric_col <- function(col, df) {
is.numeric(magrittr::extract2(df, col))
  }


