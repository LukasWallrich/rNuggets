
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note re rNuggets: Most functions in this package are alpha-versions - please treat results with care and report bugs.")
}

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom stats as.formula cor.test sd t.test lm p.adjust.methods
#' @importFrom generics tidy
#' @importFrom generics glance
#' @importFrom broom glance
#' @importFrom utils getFromNamespace
"_PACKAGE"
