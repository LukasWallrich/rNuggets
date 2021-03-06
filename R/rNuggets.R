
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note re rNuggets: Most functions in this package are alpha-versions - please treat results with care and report bugs.")
}

#' A set of helper functions for statistics and data analysis
#'
#' This package contains a diverse set of helper functions to support tasks that
#' I frequently perform as part of my research in social psychology. They are
#' primarily written for my own use, but might be helpful to others. As they cover
#' everything from statistical modeling, graphing and presentation to project management,
#' the package has a huge list of dependencies. Eventually, this might be split
#' into a few focused packages that will be more lightweight and easier to grasp.
#' @docType package
#' @name rNuggets

"_PACKAGE"

globalVariables(".")

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom stats as.formula cor.test sd t.test lm p.adjust.methods
#' @importFrom generics tidy
#' @importFrom generics glance
#' @importFrom here here
#' @importFrom utils getFromNamespace
#' @import checkmate
#' @export
generics::tidy
generics::glance
#'
#'
"_PACKAGE"

.check_req_packages <- function(x, note = "") {
  if (suppressWarnings(!all(lapply(x, requireNamespace, quietly = TRUE)))) {
    stop(paste0(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x, collapse = ", ")),
      call. = FALSE
    )
  }
}

#' @title Deprecated functions in package \pkg{rNuggets}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("<function>-deprecated")}.
#' @name rNuggets-deprecated
#' @keywords internal
NULL
