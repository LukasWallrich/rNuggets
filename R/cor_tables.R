#' Create a correlation table with summary statistics in APA style
#'
#' This function, creates (and #' optionally saves) a correlation table with
#' summary statistics. It accepts
#'
#' @param cor_matrix A correlation matrix, for example returned from \code{
#' survey_cor_matrix()}, \code{wtd_cor_matrix()}
#' @param notes List of additional notes to show under the table.
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @inheritParams sigstars
#' @param add_title Logical. Should title be added to table?
#' @source Based on the apaTables \code{apa.cor.table()} function, but adapted to
#' accept weighted correlation matrices and work with the `gt` package instead`
#' @return A table that can be printed in the RStudio console to be shown in the
#' viewer. Unless it is to be post-processed with further `gt` functions, it should
#' usually be saved by passing a filename argument.
#' @export

apa_cor_table <- function(cor_matrix, filename = NULL,
                          notes = list(NULL), stars = NULL, add_title = TRUE) {

  .check_req_packages("gt")

  df_col <- dim(cor_matrix[[1]])[2]
  number_variables <- df_col
  number_columns <- df_col - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(
    " ", number_variables,
    1
  )
  output_variable_names <- paste(as.character(1:number_variables),
                                 ". ", rownames(cor_matrix[[1]]),
                                 sep = ""
  )

  for (i in 1:number_variables) {
    output_descriptives[i, 1] <- paste0(
      sprintf("%.2f", cor_matrix$desc[i, 2]),
      " (", sprintf("%.2f", cor_matrix$desc[i, 3]), ")"
    )
    for (j in 1:number_variables) {
      if ((j < i)) {
        cor.r <- cor_matrix$cors[i, j]
        cor.p <- cor_matrix$p.values[i, j]
        cor.se <- cor_matrix$std.err[i, j]
        output_cor[i, j] <- paste(.fmt_cor(cor.r), sigstars(cor.p, stars))
        cor_ci_string <- paste0(cor.r - 2 * cor.se, cor.r + 2 * cor.se)
        output_ci[i, j] <- paste0(
          '<span style="font-size:80%">',
          "[", .fmt_cor(cor.r - 2 * cor.se), ", ", .fmt_cor(cor.r + 2 * cor.se), "]",
          "</span>"
        )
      }
    }
  }

  cor_cells <- paste(output_cor, output_ci, sep = "<br />")
  dim(cor_cells) <- dim(output_cor)

  cells <- cbind(
    matrix(output_variable_names, ncol = 1),
    output_descriptives, cor_cells
  )

  colnames(cells) <- c("Variable", "desc", seq_len(length(output_variable_names) - 1))

  cells_df <- tibble::as_tibble(cells)

  tab <- cells_df %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::everything())

  notes %<>% c("*M* and *SD* are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation.")

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)
  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }

  if(add_title) {
  tab <- tab %>% gt::tab_header(
    title = "Means, standard deviations, and correlations with confidence intervals"
  )
  }

  tab <- tab %>% gt::cols_label(desc = gt::md("*M (SD)*"))
  if (!is.null(filename)) {
    gt::gtsave(tab, filename)
  }
  else {
    return(tab)
  }
}



#' Calculates correlation matrix with significance tests and descriptives
#'
#' Calculates the correlation matrix between the numeric variables in a given dataframe and
#' includes descriptives (mean and standard deviation) - ready for creating a nice table with \code{\link{apa_cor_table}}
#'
#' @param x Dataframe of variables that can be coerced to numeric.
#' @param var_names A named character vector with the names that should be displayed
#' for variables. If NULL, then the variables are not renamed.
#' @inheritParams psych::corr.test
#' @inheritDotParams psych::corr.test
#' @return A list including the correlation matrix, p-values, standard errors and descriptives
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @export


cor_matrix <- function(x,
                       var_names = NULL,
                       method = c("pearson", "spearman", "kendall"),
                       adjust = "none",
                       ...) {

  x %<>% dplyr::select_if(is.numeric)

  # Compute correlation matrix
  correlation_matrix <- psych::corr.test(x, method = method[1])
  cors <- correlation_matrix$r # Matrix of correlation coeficients
  p.values <- correlation_matrix$p # Matrix of p-value
  std.err <- correlation_matrix$se # Matrix of p-value
  t.values <- correlation_matrix$t # Matrix of p-value

  #Copy (possibly) adjusted p-values into lower half that will be used by apa_cor_table()
  p.values[lower.tri(p.values)] <- t(p.values)[lower.tri(p.values)]


  desc_stat <- x %>%
    psych::describe() %>%
    data.frame() %>%
    tibble::rownames_to_column("var") %>%
    dplyr::select(.data$var, M = .data$mean, SD = .data$sd)

  corM <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, desc = desc_stat)

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
    corM$desc <- corM$desc[match(corM$desc$var, used_vars), ]
  }

  corM
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
#' @export
#'

survey_cor_matrix <- function(svy_df, var_names) {
  .check_req_packages(c("jtools", "survey", "srvyr", "weights"))

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

  if (nrow(cor_matrix$desc) == 0) {
    stop("No numeric columns found - check your input and that you have
         installed the most recent dplyr version.", call. = FALSE)
  }

  if (!is.null(var_names)) {
    cor_matrix[c(1, 4:6)] <- purrr::map(cor_matrix[c(1, 4:6)], function(x) {
      rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
      colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
      x
    })
    used_vars <- intersect(var_names, rownames(cor_matrix[[1]]))
    cor_matrix[c(1, 4:6)] <- purrr::map(cor_matrix[c(1, 4:6)], function(x) x[used_vars, used_vars])
    cor_matrix$desc$var %<>% stringr::str_replace_all(var_names)
    cor_matrix$desc <- cor_matrix$desc[match(used_vars, cor_matrix$desc$var), ]
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
#' @export

wtd_cor_matrix_mi <- function(mi_list, weights, var_names = NULL) {
  .check_req_packages(c("survey", "srvyr", "mitools", "mice"))

  weights <- rlang::enquo(weights)

  mi_list <- purrr::map(mi_list, dplyr::select_if, is.numeric)

  mi_list <- purrr::map(mi_list, dplyr::select, !!weights, dplyr::everything(), -dplyr::matches("^\\."))

  variables <- names(mi_list[[1]])
  variables <- variables[-1]
  ct <- length(variables)

  df <- NULL

  for (i in 1:(ct - 1)) {
    for (j in (i + 1):ct) {
      if (i != j) {
        ii <- variables[i]
        jj <- variables[j]
        mi_selected <- purrr::map(mi_list, magrittr::extract, c(jj, ii, dplyr::as_label(weights)))
        mi_selected <- purrr::map(mi_selected, dplyr::rename, x = 1, y = 2)
        # browser()
        cor.ii.jj <- purrr::map(mi_selected, do.call, what = .wtd_cor_test_lm)
        df <- rbind(df, data.frame(x = ii, y = jj, mice::pool(cor.ii.jj) %>% summary() %>% magrittr::extract(c("estimate", "p.value", "std.error", "statistic")) %>% magrittr::extract(2, )))
      }
    }
  }


  to_matrix <- function(df, names, value) {
    m <- matrix(0, length(names), length(names))
    m[as.matrix(df %>% magrittr::extract(c("row", "column")))] <- df[[value]]
    rownames(m) <- names
    colnames(m) <- names
    diag(m) <- 1
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

  corM <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, desc = desc, tests = df)

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
    corM$desc <- corM$desc[match(used_vars, corM$desc$var), ]
  }

  corM
}

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  lm(scale(y) ~ scale(x), weights = wt)
}