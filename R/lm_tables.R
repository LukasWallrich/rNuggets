
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
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardisation is also automatically added.
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -statistic_override -conf_level -stars
#' @export

lm_with_std <- function(mod, std_mod, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, notes = list(NULL), ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr"))

  tidy.mira <- getFromNamespace("tidy.mira", "modelsummary")
  glance.mira <- getFromNamespace("glance.mira", "modelsummary")

  if ((is.list(mod) | is.list(std_mod)) & !(length(mod) == length(std_mod))) {
    stop("Same number of models need to be included in mod and std_mod arguments")
  }

  if (!is.null(model_names) & !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
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
  gof <- purrr::map(mod, modelsummary:::extract_gof, fmt, gof_map)
  gof_map$omit <- TRUE

  SEs <- list()
  CIs <- list()
  mods <- list()
  mod_tidy <- list()
  std_mod_tidy <- list()
  stat_list <- list()

  for (i in seq_len(length(mod))) {
    mod_tidy[[i]] <- generics::tidy(mod[[i]])
    SEs[[i]] <- paste0("(", sprintf(fmt, mod_tidy[[i]]$std.error), ")", sigstars(mod_tidy[[i]]$p.value, pad_html = TRUE))
    names(SEs[[i]]) <- mod_tidy[[i]]$term

    std_mod_tidy[[i]] <- generics::tidy(std_mod[[i]], conf.int = TRUE, conf.level = conf_level)
    CIs[[i]] <- paste0("[", sprintf(fmt, std_mod_tidy[[i]]$conf.low), ", ", sprintf(fmt, std_mod_tidy[[i]]$conf.high), "]")
    names(CIs[[i]]) <- std_mod_tidy[[i]]$term

    mods[[i * 2 - 1]] <- mod[[i]]
    stat_list[[i * 2 - 1]] <- SEs[[i]]
    mods[[i * 2]] <- std_mod[[i]]
    stat_list[[i * 2]] <- CIs[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  col_labels <- rep(list(gt::md("*<center>B (SE)</center>*"), gt::md("*<center>&beta; [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))


  if ("rN_std" %in% class(std_mod[[1]]) | ("mira" %in% class(std_mod[[1]]) & "rN_std" %in% class(std_mod[[1]][[1]]))) {
    notes %<>% c("Given that dummy variables loose their interpretability when standardised (Fox, 2015), &beta; for dummy variables are semi-standardised, indicating the impact of that dummy on the standardized outcome variable.")
  }

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)

  tab <- modelsummary::msummary(mods, statistic_override = stat_list, statistic_vertical = statistic_vertical, gof_map = gof_map, ...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>% gt::cols_align("right", dplyr::everything()) %>% gt::cols_align("left", columns = 1)

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], ""**"")), columns = (2 * i):(2 * i + 1))
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

  temp_file <- tempfile()
  tab %>% htmltools::as.tags() %>%  htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  if (!is.null(filename)) {
    readr::write_file(code, filename)
  }
  else {
    return(list(gt_tab = tab, html_code = code))
  }
}

.lm_F_test <- function(mod) {
  if("lm" %in% class(mod$analyses[[1]])) return(mira.lm_F_test(mod))
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
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* = ",
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

.check_req_packages <- function(x, note = "") {
  if (suppressWarnings(!all(lapply(x, requireNamespace, quietly = TRUE)))) {
    stop(paste0(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x, collapse = ", ")),
      call. = FALSE
    )
  }
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
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardisation is also automatically added.
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -statistic_override -conf_level -stars

polr_with_std <- function(mod, std_mod, OR = TRUE, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, notes = list("Given that dummy variables loose their interpretability when standardised (Fox, 2015), &beta; for dummy variables are semi-standardised, indicating the impact of that dummy on the standardized outcome variable."), ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr", "pscl"))

  tidy.mira <- getFromNamespace("tidy.mira", "modelsummary")
  glance.mira <- getFromNamespace("glance.mira", "modelsummary")

  if ((is.list(mod) | is.list(std_mod)) & !(length(mod) == length(std_mod))) {
    stop("Same number of models need to be included in mod and std_mod arguments")
  }

  if (!("list" %in% class(mod))) mod <- list(mod)
  if (!("list" %in% class(std_mod))) std_mod <- list(std_mod)

  if (!is.null(model_names) & !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  mod <- purrr::map(mod, function(x) {
    if(class(x)[1] == "polr") add_class(x, "polr_p") else x
  })

  std_mod <- purrr::map(std_mod, function(x) {
    if(class(x)[1] == "polr") add_class(x, "polr_p") else x
  })

  if(OR) {
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
  gof <- purrr::map(mod, modelsummary:::extract_gof, fmt, gof_map)
  gof_map$omit <- TRUE

  CIs <- list()
  CIs_std <- list()
  mods <- list()
  mod_tidy <- list()
  std_mod_tidy <- list()
  stat_list <- list()

  for (i in seq_len(length(mod))) {
    mod_tidy[[i]] <- generics::tidy(mod[[i]], conf.int = TRUE, conf.level = conf_level)
    CIs[[i]] <- paste0("[", sprintf(fmt, mod_tidy[[i]]$conf.low), ", ", sprintf(fmt, mod_tidy[[i]]$conf.high), "] ", sigstars(mod_tidy[[i]]$p.value, pad_html = TRUE))
    names(CIs[[i]]) <- mod_tidy[[i]]$term

    std_mod_tidy[[i]] <- generics::tidy(std_mod[[i]], conf.int = TRUE, conf.level = conf_level)
    CIs_std[[i]] <- paste0("[", sprintf(fmt, std_mod_tidy[[i]]$conf.low), ", ", sprintf(fmt, std_mod_tidy[[i]]$conf.high), "] ", sigstars(mod_tidy[[i]]$p.value, pad_html = TRUE))
    names(CIs_std[[i]]) <- std_mod_tidy[[i]]$term

    mods[[i * 2 - 1]] <- mod[[i]]
    stat_list[[i * 2 - 1]] <- CIs[[i]]
    mods[[i * 2]] <- std_mod[[i]]
    stat_list[[i * 2]] <- CIs_std[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  if(OR) {
  col_labels <- rep(list(gt::md("*<center>OR [95% CI]</center>*"), gt::md("*<center>Stand. OR [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  } else {
    col_labels <- rep(list(gt::md("*<center>Coefs [95% CI]</center>*"), gt::md("*<center>Stand. Coefs [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  }

  notes %<>% c(.make_stars_note())

  #~~~~~~~~~~~~~~~
  return(TRUE)

  tab <- modelsummary::msummary(mods, statistic_override = stat_list, statistic_vertical = statistic_vertical, gof_map = gof_map, ...) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(.list = col_labels) %>% gt::cols_align("right", dplyr::everything()) %>% gt::cols_align("left", columns = 1)

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], ""**"")), columns = (2 * i):(2 * i + 1))
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
    if(class(mods[[i]])[1] %in% c("glm", "polr", "multinorm")) {
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
  tab %>% htmltools::as.tags() %>%  htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  if (!is.null(filename)) {
    readr::write_file(code, filename)
  }
  else {
    return(list(gt_tab = tab, html_code = code))
  }
}

