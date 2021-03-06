#' Creates a summary table comparing standardised and non-standardised linear models
#'
#' This function creates a summary table for lm models (including mice::mira objects
#' containing lm-models) that shows a standardised and non-standardised version of the model
#' side-by-side. Several pairs of such models can be compared side-by-side.
#'
#' @param mod A lm-model/mira object of lm models, with variables not standardised (or a list of such models)
#' @param std_mod A lm-model/mira object of lm models, with standardised variables. Can be
#' created with \code{\link{lm_std}} (or a list of such models)
#' @param conf_level Confidence level to use for confidence intervals, defaults to .95
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @param model_names If several pairs of models are to be plotted side by side, indicate the label for each *pair* here
#' @param show_nimp Logical. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic
#' @param R2_change Logical. Report R2 change and F-test to compare models. Only implemented for comparing two pairs of models.
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardisation is also automatically added.
#' @param apa_style Logical, should APA-style formatting be applied
#' @param statistic_vertical Should standard errors and CIs be shown below coefficients? Defaults to horizontal layout
#' @param stars Named vector of significance stars and their thresholds, check `rNuggets:::std_stars_pad` for default.
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -conf_level -stars -vcov
#' @export

lm_with_std <- function(mod, std_mod, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, R2_change = FALSE, notes = list(NULL), apa_style = TRUE, stars = std_stars_pad, ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr"))



  if ((class(mod)[1] == "list" | class(std_mod)[1] == "list") & !(length(mod) == length(std_mod))) {
    stop("Same number of models need to be included in mod and std_mod arguments")
  }

  if (!is.null(model_names) & !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  if (R2_change == TRUE & !length(mod) == 2) {
    stop("R2 change can only be included in tables with exactly two pairs of models")
  }


  if (!("list" %in% class(mod))) mod <- list(mod)
  if (!("list" %in% class(std_mod))) std_mod <- list(std_mod)


  gof_map <- tibble::tribble(
    ~raw, ~clean, ~fmt, ~omit,

    "nobs", "*N*", "%.0f", FALSE,
    "r.squared", "R<sup>2</sup>", "%.3f", FALSE,
    "adj.r.squared", "Adj.R<sup>2</sup>", "%.3f", FALSE,
    "AIC", "AIC", "%.1f", TRUE,
    "BIC", "BIC", "%.1f", TRUE,
    "logLik", "Log.Lik.", "%.3f", TRUE,
    "deviance", "Deviance", "%.2f", TRUE,
    "df.residual", "DF Resid", "%.0f", TRUE,
    "df.null", "DF Null", "%.0f", TRUE,
    "sigma", "Sigma", "%.3f", TRUE,
    "statistic", "Statistics", "%.3f", TRUE,
    "p.value", "p", "%.3f", TRUE,
    "df", "DF", "%.0f", TRUE,
    "null.deviance", "Deviance Null", "%.2f", TRUE,
    "nimp", "No of Imputations", "%.0f", TRUE,
  )

  if (show_nimp) gof_map[nrow(gof_map), ncol(gof_map)] <- FALSE

  extract_gof <- getFromNamespace("extract_gof", "modelsummary")

  gof <- purrr::map(mod, extract_gof, fmt, gof_map)
  gof_map$omit <- TRUE

  SEs <- list()
  CIs <- list()
  mods <- list()
  mod_tidy <- list()
  std_mod_tidy <- list()
  stat_list <- list()

  for (i in seq_len(length(mod))) {
    mod_tidy[[i]] <- tidy(mod[[i]])
    SEs[[i]] <- paste0("(", sprintf(fmt, mod_tidy[[i]]$std.error), ")")
    names(SEs[[i]]) <- mod_tidy[[i]]$term

    std_mod_tidy[[i]] <- tidy(std_mod[[i]], conf.int = TRUE, conf.level = conf_level)
    CIs[[i]] <- paste0("[", sprintf(fmt, std_mod_tidy[[i]]$conf.low), ", ", sprintf(fmt, std_mod_tidy[[i]]$conf.high), "]")
    names(CIs[[i]]) <- std_mod_tidy[[i]]$term

    mods[[i * 2 - 1]] <- mod[[i]]
    stat_list[[i * 2 - 1]] <- SEs[[i]]
    mods[[i * 2]] <- std_mod[[i]]
    stat_list[[i * 2]] <- CIs[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  col_labels <- rep(list(gt::md("*<center>B (SE)</center>*"), gt::md("*<center>&beta; [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))


  if ("rN_std" %in% class(std_mod[[1]]) | ("mira" %in% class(std_mod[[1]]) & "rN_std" %in% class(std_mod[[1]]$analyses[[1]]))) {
    notes %<>% c("Given that dummy variables lose their interpretability when standardised (Fox, 2015), &beta; for dummy variables are semi-standardised, indicating the impact of that dummy on the standardised outcome variable.")
  }

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)

if (statistic_vertical) {
  tab <- modelsummary::msummary(mods, output = "gt", vcov = stat_list, rep(c(
    "{estimate} {stars}", "{estimate}"), length(mod)), fmt = fmt, gof_omit = ".*", stars = stars, ...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>%
    gt::cols_align("right", dplyr::everything()) %>%
    gt::cols_align("left", columns = 1) %>%
    gt:::dt_source_notes_set("") #Remove std star note
} else {
  tab <- modelsummary::msummary(mods, output = "gt", statistic = NULL, estimate = rep(c(
    "{estimate} ({std.error}){stars}",
    "{estimate} [{conf.low}, {conf.high}]"), length(mod)), fmt = fmt, gof_omit = ".*", stars = stars,...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>%
    gt::cols_align("right", dplyr::everything()) %>%
    gt::cols_align("left", columns = 1) %>%
    gt:::dt_source_notes_set("") #Remove std star note
}

  if (apa_style) tab <- tab %>% gt_apa_style()

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], "**")), columns = (2 * i):(2 * i + 1))
    }
  }

  code <- character()


  row <- '<tr style="border-top-style: solid; border-top-width: 2px;">
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>N     </td>'

  sums <- paste(purrr::map(gof, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {x$value[x$term=="*N*"]}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>R<sup>2</sup>     </td>'

  sums <- paste(purrr::map(gof, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {.fmt_cor(as.numeric(x$value[x$term=="R<sup>2</sup>"]))}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  Fs <- purrr::map(mod, .lm_F_test)

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1"><em>F</em>-tests</td>'

  sums <- paste(purrr::map(Fs, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {gt:::md_to_html(x)}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  if (R2_change == TRUE) {


    delta_R2 <- purrr::map_chr(gof, function(x) x %>% dplyr::filter(.data$term == "R<sup>2</sup>") %>% dplyr::pull(.data$value)) %>% as.numeric() %>% diff() %>% .fmt_cor()

    x <- stats::anova(mod[[1]], mod[[2]])
    F_test <- glue_warn("<em>F</em>({x$Df[2]}, {x$Res.Df[2]}) = {x$F[2] %>% round_(2)}, <em>p</em> {x$`Pr(>F)`[2] %>% fmt_p()}")

    row <- glue_warn('<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1"><em>Change</em></td>
    <td class="gt_row gt_center" rowspan="1" colspan="4">&Delta;<em>R</em><sup>2</sup> =
                     {delta_R2}, {F_test} </td></tr>')

    code %<>% paste(row, collapse = "")

    }

  temp_file <- tempfile()
    tab %>%
    htmltools::as.tags() %>%
    htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  if (!is.null(filename)) {
    readr::write_file(code, filename)
  }
  else {
    return(list(gt_tab = tab, html_code = code))
  }
}

.lm_F_test <- function(mod) {
  if ("lm" %in% class(mod$analyses[[1]])) {
    return(mira.lm_F_test(mod))
  }
  model_summary <- summary(mod)
  f.stat <- model_summary$fstatistic[1]
  DoF <- model_summary$fstatistic[2]
  DoF_residual <- model_summary$fstatistic[3]

  p_value <- stats::pf(f.stat, DoF, DoF_residual,
    lower.tail = FALSE
  )

  fmt <- "%.2f"
  fmt_0 <- "%.0f"

  paste0(
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* ",
    fmt_p(p_value)
  )
}
#' Report F-test for significance of multiply imputed lm models
#'
#' Takes a mira object (list of lm models based on mice imputations) and returns
#' an F-test for their significance, based on \code{\link[miceadds]{micombine.F}}
#'
#' @param mod A mira object (list of lm models in `analyses` element)
#' @param return_list Logical. Should items of test be returned in a list?
#' Otherwise, a string for reporting is returned, with Markdown formatting for APA style
#' @export

mira.lm_F_test <- function(mod, return_list = FALSE) {
  .check_req_packages(c("miceadds"))
  extract_F <- function(x) {
    summary(x) %>%
      magrittr::extract2("fstatistic") %>%
      magrittr::extract(1)
  }
  F <- purrr::map_dbl(mod$analyses, extract_F)

  DoF <- summary(mod$analyses[[1]])$fstatistic[2]

  f.statistics <- miceadds::micombine.F(F, df1 = DoF, display = FALSE)
  f.stat <- f.statistics["D.numdf"]

  DoF_residual <- summary(mod$analyses[[1]])$fstatistic[3]

  p_value <- f.statistics["p.numdf"]

  if (return_list) {
    return(list(F = f.stat, DoF = DoF, DoF_residual = DoF_residual, p.value = p_value))
  }

  fmt <- "%.2f"
  fmt_0 <- "%.0f"

  paste0(
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* = ",
    fmt_p(p_value)
  )
}


#' Creates a summary table comparing standardised and non-standardised
#' proportional odd logistic regression models
#'
#' This function creates a summary table for polr models (including mice::mira objects
#' containing polr-models) that shows a standardised and non-standardised version of the model
#' side-by-side. Several pairs of such models can be compared side-by-side.
#'
#' @param mod A polr-model/mira object of polr models, with variables not standardised (or a list of such models)
#' @param std_mod A polr-model/mira object of polr models, with standardised predictor variables (or a list of such models)
#' @param conf_level Confidence level to use for confidence intervals, defaults to .95
#' @param OR Logical. Shoulds odds ratios be shown instead of typical coefficients. If TRUE, estimates are exponentiated
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @param model_names If several pairs of models are to be plotted side by side, indicate the label for each *pair* here
#' @param show_nimp Logical. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. A note is also added
#' stating that dummy variables were not scaled in standardisation. If you approached standardisation differently, that should be removed.
#' @param apa_style Logical, should APA-style formatting be applied
#' @param stars Named vector of significance stars and their thresholds, check `rNuggets:::std_stars_pad` for default.
#' @param statistic_vertical Should standard errors and CIs be shown below coefficients? Defaults to horizontal layout
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -conf_level -stars
#' @export

polr_with_std <- function(mod, std_mod, OR = TRUE, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, notes = list(), apa_style = TRUE, stars = std_stars_pad, ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr", "pscl"))

  #TK: add polr_std function and show this note only when that function was used.

  notes %<>% c("Given that dummy variables loose their interpretability when standardised (Fox, 2015), standardised OR are only shown for continuous predictors.")

  if ((is.list(mod) | is.list(std_mod)) & !(length(mod) == length(std_mod))) {
    stop("Same number of models need to be included in mod and std_mod arguments")
  }

  if (!("list" %in% class(mod))) mod <- list(mod)
  if (!("list" %in% class(std_mod))) std_mod <- list(std_mod)

  if (!is.null(model_names) & !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  mod <- purrr::map(mod, function(x) {
    if (class(x)[1] == "polr") add_class(x, "polr_p") else x
  })

  std_mod <- purrr::map(std_mod, function(x) {
    if (class(x)[1] == "polr") add_class(x, "polr_p") else x
  })

  if (OR) {
    mod <- purrr::map(mod, add_class, "exp")
    std_mod <- purrr::map(std_mod, add_class, "exp")
  }


  gof_map <- tibble::tribble(
    ~raw, ~clean, ~fmt, ~omit,

    "nobs", "*N*", "%.0f", FALSE,
    "r.squared", "R<sup>2</sup>", "%.3f", FALSE,
    "adj.r.squared", "Adj.R<sup>2</sup>", "%.3f", FALSE,
    "AIC", "AIC", "%.1f", TRUE,
    "BIC", "BIC", "%.1f", TRUE,
    "logLik", "Log.Lik.", "%.3f", TRUE,
    "deviance", "Deviance", "%.2f", TRUE,
    "df.residual", "DF Resid", "%.0f", TRUE,
    "df.null", "DF Null", "%.0f", TRUE,
    "sigma", "Sigma", "%.3f", TRUE,
    "statistic", "Statistics", "%.3f", TRUE,
    "p.value", "p", "%.3f", TRUE,
    "df", "DF", "%.0f", TRUE,
    "null.deviance", "Deviance Null", "%.2f", TRUE,
    "nimp", "No of Imputations", "%.0f", TRUE,
  )

  if (show_nimp) gof_map[nrow(gof_map), ncol(gof_map)] <- FALSE
  extract_gof <- getFromNamespace("extract_gof", "modelsummary")
  gof <- purrr::map(mod, extract_gof, fmt, gof_map)
  gof_map$omit <- TRUE

  CIs <- list()
  CIs_std <- list()
  mods <- list()
  mod_tidy <- list()
  std_mod_tidy <- list()
  stat_list <- list()

  for (i in seq_len(length(mod))) {
     mods[[i * 2 - 1]] <- mod[[i]]
    mods[[i * 2]] <- std_mod[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  if (OR) {
    col_labels <- rep(list(gt::md("*<center>OR [95% CI]</center>*"), gt::md("*<center>Stand. OR [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  } else {
    col_labels <- rep(list(gt::md("*<center>Coefs [95% CI]</center>*"), gt::md("*<center>Stand. Coefs [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  }

  notes %<>% c(.make_stars_note())

if (statistic_vertical) {
  tab <- modelsummary::msummary(mods, output = "gt", estimate = "{estimate} {stars}", statistic = "[{conf.low}, {conf.high}]", fmt = fmt, gof_omit = ".*", stars = stars, ...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>%
    gt::cols_align("right", dplyr::everything()) %>%
    gt::cols_align("left", columns = 1) %>%
    gt:::dt_source_notes_set("") #Remove std star note
} else {
  tab <- modelsummary::msummary(mods, output = "gt", statistic = NULL, estimate = "{estimate} {stars} [{conf.low}, {conf.high}]", fmt = fmt, gof_omit = ".*", stars = stars,...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>%
    gt::cols_align("right", dplyr::everything()) %>%
    gt::cols_align("left", columns = 1) %>%
    gt:::dt_source_notes_set("") #Remove std star note
}
  if (apa_style) tab <- tab %>% gt_apa_style()

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], "**")), columns = (2 * i):(2 * i + 1))
    }
  }

  code <- character()


  row <- '<tr style="border-top-style: solid; border-top-width: 2px;">
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>N     </td>'

  sums <- paste(purrr::map(gof, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {x$value[x$term=="*N*"]}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>R<sup>2</sup>     </td>'

  R2s <- numeric()

  for (i in seq_along(mods)) {
    if (class(mods[[i]])[1] %in% c("glm", "polr", "multinorm")) {
      R2s[i] <- pscl::pR2(mods[[i]]) %>% magrittr::extract("r2ML")
    } else if ("mira" %in% class(mods[[i]])) {
      R2s[i] <- mean(purrr::map_dbl(mods[[i]]$analyses, function(x) pscl::pR2(x) %>% magrittr::extract("r2ML")))
    }
  }

  sums <- paste(purrr::map(R2s, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {.fmt_cor(as.numeric(x))}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")


  temp_file <- tempfile()
  tab %>%
    htmltools::as.tags() %>%
    htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  if (!is.null(filename)) {
    readr::write_file(code, filename)
  }
  else {
    return(list(gt_tab = tab, html_code = code))
  }
}

#' @importFrom generics glance
#' @importFrom generics tidy
#' @export
generics::glance
generics::tidy

#' Tidy  multiple imputation models created with `mice`
#'
#' Note that the `mice` authors prefer to tidy `mipo` rather than `mira` objects and have now included `tidy.mipo` and `glance.mipo` into their package. The `mira` functions here are mostly retained for compatibility with my earlier code.
#'
#' @param x A `mira` object containing multiple models based on `mice` imputations.
#' @param conf.int Logical. Should confidence intervals be returned. Defaults to true.
#' @param conf.level Confidence level for intervals. Defaults to .95
#' @param ... extra arguments (not used)
#' @note
#' Available stats in result:
#' \itemize{
#'      \item estimate
#'      \item ubar
#'      \item b
#'      \item t
#'      \item dfcom
#'      \item df
#'      \item riv
#'      \item lambda
#'      \item fmi
#'      \item p.value
#'      \item conf.low (if called with conf.int = TRUE)
#'      \item conf.high (if called with conf.int = TRUE)
#' }
#'@export
tidy.mira <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  out <- summary(mice::pool(x, ...), type = "all", conf.int = conf.int, conf.level = conf.level) %>%
    dplyr::mutate(term = as.character(.data$term)) %>%
    tibble::as_tibble()
  conf_vars <- names(out)[stringr::str_detect(names(out), "%")]
  names(out)[names(out) %in% conf_vars] <- c("conf.low", "conf.high")
  out <- out %>% dplyr::select(.data$term, order(names(.)))
  return(out)
}

#' Glance a multiple imputation `mice` pooled object
#'
#' Note that the `mice` authors prefer to tidy `mipo` rather than `mira` objects and have now included `tidy.mipo` and `glance.mipo` into their package. The `mira` functions here are mostly retained for compatibility with my earlier code.
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used)
#' @return a tibble with one row
#'
#' @note If x contains `lm` models, R2 is included in the output
#'
#' @examples
#' \dontrun{
#' library(mice)
#' data <- airquality
#' data[4:10, 3] <- rep(NA, 7)
#' data[1:5, 4] <- NA
#' tmp <- mice(data, m = 5, seed = 500, printFlag = FALSE)
#' mod <- with(tmp, lm(Ozone ~ Solar.R + Wind))
#' glance(mod)
#' }
#' @export
glance.mira <- function(x, ...) {
  out <- tibble::tibble("nimp" = length(x$analyses))
  out$nobs <- tryCatch(stats::nobs(x$analyses[[1]]), error = function(e) NULL)
  if (class(x$analyses[[1]])[1] == "lm") {
    out$r.squared <- mice::pool.r.squared(x, adjusted = FALSE)[1]
    out$adj.r.squared <- mice::pool.r.squared(x, adjusted = TRUE)[1]
  }
  return(out)
}

#' Helper function to enable tidy.lm to be used on lm_std() models
#'
#' Strips rN_std class and calls tidy() again
#' @param x An object with class rN_std
#' @param ... arguments passed on to tidy method
#' @export

tidy.rN_std <- function(x, ...) {
  class(x) <- class(x)[class(x) != "rN_std"]
  generics::tidy(x, ...)
}

#' Helper function to style gt-table in APA style
#'
#' This function takes a `gt` table object and changes font-type, borders etc
#' to align with APA style.
#'
#' @param gt_table A gt-table
#' @param fmt_labels_md Should row and column labels be formatted with markdown/HTML (Defaults to TRUE)
#' @source Created by Philip Parker, https://gist.github.com/pdparker/1b61b6d36d09cb295bf286a931990159. Slightly expanded here.
#' @export


gt_apa_style <- function(gt_table, fmt_labels_md = TRUE) {
  out <- gt_table %>%
    gt::opt_table_lines(extent = "none") %>%
    gt::tab_options(
      heading.border.bottom.width = 2,
      heading.border.bottom.color = "black",
      heading.border.bottom.style = "solid",
      table.border.top.color = "white",
      table_body.hlines.color = "white",
      table_body.border.top.color = "black",
      table_body.border.top.style = "solid",
      table_body.border.top.width = 1,
      heading.title.font.size = 12,
      table.font.size = 12,
      heading.subtitle.font.size = 12,
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 1,
      table_body.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = 1
    ) %>%
    gt::opt_table_font(font = "times")

  if (fmt_labels_md) out <- fmt_labels_md(out)
  out
}

#' A convenience function to render markdown to html in row and column labels
#'
#' @param tab a `gt` table object
#' @param position character string determines wither row, column or both
#'   labels should be rendered.
#' @note This function only works for HTML output, since the `gt` render tools
#' are less developed for LaTeX and RTF output.
#' @source Developed with Vincent Arel-Bundock and first included in `modelsummary`-package

fmt_labels_md <- function(tab, position = c('both', 'row', 'column')) {
  out <- tab
  if (match.arg(position) %in% c('both', 'row')) {
    out <- gt::fmt_markdown(out, columns = 1)
  }
  if (match.arg(position) %in% c('both', 'column')) {

    f <- function(x) stats::setNames(lapply(unlist(x$`_boxhead`$column_label), gt::md), names(x$`_data`))
    out <- gt::cols_label(out, .list = f(out))
  }
  return(out)
}
