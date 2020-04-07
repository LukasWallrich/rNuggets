#' Create a correlation table with summary statistics in APA style
#'
#' This function, creates (and #' optionally saves) a correlation table with
#' summary statistics. It accepts
#'
#' @param cor_matrix A correlation matrix, for example returned from \code{
#' survey_cor_matrix()}, \code{wtd_cor_matrix()}
#' @param notes List of additional notes to show under the table.
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @source Based on the apaTables \code{apa.cor.table()} function, but adapted to
#' accept weighted correlation matrices and work with the `gt` package instead`
#' @return A table that can be printed in the RStudio console to be shown in the
#' viewer. Unless it is to be post-processed with further `gt` functions, it should
#' usually be saved by passing a filename argument.
#'
apa_cor_table <- function (cor_matrix, filename = NULL,
          notes = list(NULL)) {
  req_packages <- c("gt")
  if (suppressWarnings(!all(lapply(req_packages, requireNamespace, quietly=TRUE)))) {
    stop(paste0("Some required packages are not installed. Make sure you have
               these packages: ", paste0(req_packages, collapse = ", ")),
         call. = FALSE)
  }

  df_col <- dim(cor_matrix[[1]])[2]
  number_variables <- df_col
  number_columns <- df_col - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(" ", number_variables,
                                1)
  output_variable_names <- paste(as.character(1:number_variables),
                                 ". ", rownames(cor_matrix[[1]]), sep = "")

  for (i in 1:number_variables) {
    output_descriptives[i, 1] <- paste0(sprintf("%.2f", cor_matrix$desc[i, 2]),
                                        " (", sprintf("%.2f", cor_matrix$desc[i, 3]), ")")
    for (j in 1:number_variables) {
      if ((j < i)) {
        cor.r <- cor_matrix$cors[i, j]
        cor.p <- cor_matrix$p.values[i, j]
        cor.se <- cor_matrix$std.err[i, j]
        output_cor[i, j] <- paste(.fmt_cor(cor.r), sigstars(cor.p))
        cor_ci_string <- paste0(cor.r-2*cor.se , cor.r+2*cor.se)
        output_ci[i, j] <- paste0('<span style="font-size:80%">',
          "[", .fmt_cor(cor.r-2*cor.se), ", ", .fmt_cor(cor.r+2*cor.se), "]",
          "</span>")
      }
    }
  }

  cor_cells <- paste(output_cor, output_ci)
  dim(cor_cells) <- dim(output_cor)

  cells <- cbind(matrix(output_variable_names, ncol = 1),
                        output_descriptives, cor_cells)

  colnames(cells) <- c("Variable", "M (SD)",seq_len(length(output_variable_names)-1))

  cells_df <- tibble::as_tibble(cells)

  tab <- cells_df %>% gt::gt() %>% gt::fmt_markdown(columns = gt::everything())

  notes %<>% c("*M* and *SD* are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation.")


notes %<>% c(.make_stars_note())

notes <- Filter(Negate(is.null), notes)
for (i in seq_along(notes)) {
  tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
}

tab <- tab %>% gt::tab_header(
  title = "Means, standard deviations, and correlations with confidence intervals"
)
if (!is.null(filename)) {
  gt::gtsave(tab, filename)
}
else {
  return(tab)
}
}



#' Create a correlation matrix from survey data with summary statistics
#'
#' This function wraps jtools::svycor() so that it works in a srvyr-pipeline,
#' runs bootstrapped significance-tests and calculates weighted summary
#' statistics. Only numeric variables are included in the result.
#'
#' @param svy_df A survey object created with the survey or srvyr package. Only
#' numeric variables will be included in the result.
#' @param var_names Named character vector to rename variables to - most helpful
#' if return is to be passed to some print function
#' @return A correlation matrix list in the format provided by
#' \code{jtools::svycor()} with the addition of a \code{desc}-element with means
#' and standard deviations of the variables.
#'
#'

survey_cor_matrix <- function(svy_df, var_names) {
  req_packages <- c("jtools", "survey", "srvyr")
  if (suppressWarnings(!all(lapply(req_packages, requireNamespace, quietly=TRUE)))) {
    stop(paste0("Some required packages are not installed. Make sure you have
               these packages: ", paste0(req_packages, collapse = ", ")),
         call. = FALSE)
  }
  cor_matrix <- jtools::svycor(~., svy_df, na.rm = TRUE, sig.stats = TRUE)

cor_matrix$desc <- svy_df %>%
  srvyr::select_if(is.numeric) %>%
  srvyr::summarise_all(.funs = list(`1M` = srvyr::survey_mean, `1SD` = srvyr::survey_var), na.rm = TRUE) %>%
  dplyr::select(!dplyr::matches("_se")) %>%
  tidyr::gather(key = "key", value = "value") %>%
  tidyr::separate(.data$key, into = c("var", "statistic"), sep = "_1") %>%
  tidyr::spread(.data$statistic, .data$value) %>%
  dplyr::mutate(SD = sqrt(.data$SD)) %>%
  dplyr::arrange(match(.data$var, rownames(cor_matrix[[1]])))

  if (nrow(cor_matrix$desc)==0) {
    stop("No numeric columns found - check your input and that you have
         installed the most recent dplyr version.",  call. = FALSE)
  }

if (!is.null(var_names)) {
  cor_matrix[c(1,4:6)] <- purrr::map(cor_matrix[c(1,4:6)], function(x) {
    rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
    colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
    x
  })
  used_vars <- intersect(var_names, rownames(cor_matrix[[1]]))
  cor_matrix[c(1,4:6)] <- purrr::map(cor_matrix[c(1,4:6)], function(x) x[used_vars, used_vars])
  cor_matrix$desc$var %<>% stringr::str_replace_all(var_names)
  cor_matrix$desc %<>% extract(used_vars,)
}


  cor_matrix
}

#' Create a correlation matrix from multiply imputed data with weights
#'
#' This function takes an imputationList with a vector of weights and returns
#' a correlation matrix for all numeric variables as well as a list of
#' descriptives that pools the results across all imputations.
#'
#' Variables starting with . are dropped, as these are likely to be .imp and .id
#' from mice. If you want correlations for such variables, rename them.
#'
#' @param mi_list A list of dataframes of multiple imputation results
#' @param weights A variable within mi_list that gives the survey weights
#' @param var_names A named character vector with the names that should be displayed
#' for variables. To facilitate post-processing, correlations with original variable
#' names are returned in the `tests` element.
#' @return A correlation matrix list similar to the format provided by
#' \code{jtools::svycor()} with the addition of a \code{desc}-element with means
#' and standard deviations of the variables.
#' @source Takes some code from the \code{miceadds::micombine.cor} function,
#' but adapted to use weights and return in the format accepted by
#' \code{apa.cor.table.survey}
#'

wtd_cor_matrix_mi <- function(mi_list, weights, var_names = NULL) {
  req_packages <- c("survey", "srvyr", "mitools", "mice")
  if (suppressWarnings(!all(lapply(req_packages, requireNamespace, quietly=TRUE)))) {
    stop(paste0("Some required packages are not installed. Make sure you have
               these packages: ", paste0(req_packages, collapse = ", ")),
         call. = FALSE)
  }
  weights <- rlang::enquo(weights)

  mi_list <- purrr::map(mi_list, dplyr::select_if, is.numeric)

  mi_list <- purrr::map(mi_list, dplyr::select, !!weights, dplyr::everything(), -dplyr::matches("^\\."))

  variables <- names(mi_list[[1]])
  variables <- variables[-1]
  ct <- length(variables)

  df <- NULL

  for (i in 1:(ct-1)) {
    for (j in (i + 1):ct) {
      if (i != j) {
        ii <- variables[i]
        jj <- variables[j]
          mi_selected <- purrr::map(mi_list, magrittr::extract, c(jj, ii, dplyr::as_label(weights)))
          mi_selected <- purrr::map(mi_selected, dplyr::rename, x = 1, y = 2)
          #browser()
          cor.ii.jj <- purrr::map(mi_selected, do.call, what=.wtd_cor_test_lm)
          df <- rbind(df, data.frame(x = ii, y = jj, mice::pool(cor.ii.jj) %>% summary()%>%magrittr::extract(c("estimate", "p.value", "std.error", "statistic")) %>% magrittr::extract(2,)))

          }
        }
      }


  to_matrix <- function(df, names, value) {
    m <- matrix(0, length(names), length(names))
    m[as.matrix(df %>% magrittr::extract(c("row", "column")))] <- df[[value]]
    rownames(m) <- names
    colnames(m) <- names
    diag(m)<-1
    empty <- lower.tri(m)
    m[empty] <- t(m)[empty]
    m
  }

  df %<>% dplyr::mutate(row = match(.data$x, variables), column = match(.data$y, variables))
  cors <- to_matrix(df, variables, "estimate")
  std.err <- to_matrix(df, variables, "std.error")
  p.values <- to_matrix(df, variables, "p.value")
  t.values <- to_matrix(df, variables, "statistic")

  imp_svy <- survey::svydesign(~1, weights = as.formula(paste0("~", dplyr::as_label(weights))), data = mitools::imputationList(mi_list))


  desc <- NULL
  for (i in 1:ct) {
    M <- mitools::MIcombine(with(imp_svy, survey::svymean(as.formula(paste0("~", variables[i])), design = .design)))[[1]]
    SD <- sqrt(mitools::MIcombine(with(imp_svy, survey::svyvar(as.formula(paste0("~", variables[i])), design = .design)))[[1]])
    desc <- rbind(desc, data.frame(var = variables[i], M = M, SD = SD))
    }

  corM <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, desc = desc, tests=df)

  if (!is.null(var_names)) {
    corM[1:4] <- purrr::map(corM[1:4], function(x) {
      rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
      colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
      x
    })
    used_vars <- intersect(var_names, rownames(corM[[1]]))
    corM[1:4] <- purrr::map(corM[1:4], function(x) x[used_vars, used_vars])
    rownames(corM$desc) <- rownames(corM$desc) %>% stringr::str_replace_all(var_names)
    corM$desc$var %<>% stringr::str_replace_all(var_names)
    corM$desc %<>% extract(used_vars,)
  }

  corM

}

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  lm(scale(y)~scale(x), weights = wt)
}


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
#' @param show_m Logical. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardisation is also automatically added.
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -statistic_override -conf_level -stars

lm_with_std <- function(mod, std_mod, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_m = FALSE, notes = list(NULL), ...) {

 .check_req_packages(c("modelsummary", "gt"))

  if ((is.list(mod) | is.list(std_mod)) & !(length(mod) == length(std_mod))) {
    stop("Same number of models need to be included in mod and std_mod arguments")
  }

  if (!is.null(model_names) & !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  if (!is.list(mod)) mod <- list(mod)
  if (!is.list(std_mod)) std_mod <- list(std_mod)


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
    "m", "No of Imputations", "%.0f", TRUE,
  )



  if (show_m) gof_map[nrow(gof_map), ncol(gof_map)] <- FALSE

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
    SEs[[i]] <- paste0("(", sprintf(fmt, mod_tidy[[i]]$std.error), ")", trimws(sigstars(mod_tidy[[i]]$p.value)))
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

  col_labels <- rep(list(gt::md("*B (SE)*"), gt::md("*&beta; [95% CI]*")), times = length(mod)) %>% stats::setNames(names(mods))


  if ("rN_std" %in% class(std_mod[[1]]) | ("mira" %in% class(std_mod[[1]]) & "rN_std" %in% class(std_mod[[1]][[1]]))) {
    notes %<>% c("Given that dummy variables loose their interpretability when standardised (Fox, 2015), &beta; for dummy variables are semi-standardised, indicating the impact of that dummy on the standardized outcome variable.")
  }

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)


  tab <- modelsummary::msummary(mods, statistic_override = stat_list, statistic_vertical = statistic_vertical, gof_map = gof_map, gof_omit = c("*N*"), ...) %>%
    modelsummary:::fmt_labels_md("row") %>%
    gt::cols_label(.list = col_labels)

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }

  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      tab <- tab %>% gt::tab_spanner(model_names[i], columns = (2 * i):(2 * i + 1))
    }
  }

  browser()

  F_row <- '<tr>
    <td class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">       </td>'



  if (!is.null(filename)) {
    gt::gtsave(tab, filename)
  }
  else {
    return(tab)
  }
}

.lm_F_test <- function(mod) {
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
#'Report F-test for significance of multiply imputed lm models
#'
#'Takes a mira object (list of lm models based on mice imputations) and returns
#'an F-test for their significance, based on \code{\link[miceadds]{micombine.F}}
#'
#' @param mod A mira object (list of lm models in `analyses` element)
#' @param return_list Logical. Should items of test be returned in a list?
#' Otherwise, a string for reporting is returned, with Markdown formatting.
#'
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

  if (return_list)
    return(list(F = f.stat, DoF = DoF, DoF_residual = DoF_residual, p.value = p_value))

  fmt <- "%.2f"
  fmt_0 <- "%.0f"

  paste0(
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* = ",
    fmt_p(p_value)
  )
}

.check_req_packages <- function(x) {

  if (suppressWarnings(!all(lapply(x, requireNamespace, quietly = TRUE)))) {
    stop(paste0("Some required packages are not installed. Make sure you have
               these packages: ", paste0(x, collapse = ", ")),
         call. = FALSE
    )
  }
}
