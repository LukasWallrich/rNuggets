
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note re rNuggets: Most functions in this package are alpha-versions - please treat results with care and report bugs.")
}

globalVariables(".")

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom stats as.formula cor.test sd t.test lm p.adjust.methods
#' @importFrom generics tidy
#' @importFrom generics glance
#' @importFrom broom glance
#' @importFrom utils getFromNamespace
#' @export
generics::tidy
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
