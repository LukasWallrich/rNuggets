#' Create a correlation table with summary statistics in APA style
#'
#' This function, creates (and optionally saves) a correlation table with
#' summary statistics. It accepts correlation matrices from various functions
#' in this package as its first argument
#'
#' @param cor_matrix A correlation matrix, for example returned from
#' \code{cor_matrix()}, \code{survey_cor_matrix()}, or \code{wtd_cor_matrix_mi()}
#' @param ci Method to create CI - default is to use any given in the cor_matrix,
#' and otherwise to compute them using z-transformations. The simple SE method should not be used, but is provided for compatibility.
#' @param n Number of observations to calculate confidence intervals - only needed
#' if cor_matrix does not contain degrees of freedom (df) and confidence intervals are to
#' be calulcated using z-transformations
#' @param notes List of additional notes to show under the table.
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @inheritParams sigstars
#' @param add_title Should title be added to table? Set to TRUE for default title or to character for custom title
#' @param extras Tibble of additional columns to be added after the descriptives column - needs to be sorted in the same order as the `desc` element in the cor_matrix
#' @param apa_style Logical, should APA-style formatting be applied
#' @source Based on the apaTables \code{apa.cor.table()} function, but adapted to
#' accept weighted correlation matrices and work with the `gt` package instead. Code for calculation
#' of confidence intervals adapted from https://medium.com/@shandou/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d`
#' @return A table that can be printed in the RStudio console to be shown in the
#' viewer. Unless it is to be post-processed with further `gt` functions, it should
#' usually be saved by passing a filename argument.
#' @export

apa_cor_table <- function(cor_matrix, ci = c("given", "z_transform", "simple_SE"), n = NULL, filename = NULL,
                          notes = list(NULL), stars = NULL, add_title = FALSE, extras = NULL, apa_style = TRUE) {
  if (add_title) add_title <- "Means, standard deviations, and correlations with confidence intervals"

  assert_tibble(extras, null.ok = TRUE)

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
  if (!is.null(cor_matrix[["ci.low"]]) & "given" %in% ci) {
    message("Confidence intervals are extracted from the correlation matrix")
    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      if (!is.null(cor_matrix[["ci.low"]])) {
        return(cor_matrix[["ci.low"]][i, j])
      }
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      if (!is.null(cor_matrix[["ci.high"]])) {
        return(cor_matrix[["ci.high"]][i, j])
      }
    }
  } else if ("z_transform" %in% ci & !(is.null(cor_matrix[["df"]]) & is.null(n))) {
    message("Confidence intervals are based on Fisher's r to
            z transformation.")

    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      z_prime <- .5 * log((1 + cor.r) / (1 - cor.r))
      n <- df + 1
      CI_low <- z_prime - 1.96 * 1 / sqrt(n - 3)
      tanh(CI_low)
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      z_prime <- .5 * log((1 + cor.r) / (1 - cor.r))
      n <- df + 1
      CI_low <- z_prime + 1.96 * 1 / sqrt(n - 3)
      tanh(CI_low)
    }

    if (is.null(cor_matrix[["df"]])) {
      cor_matrix$df <- cor_matrix$cors
      cor_matrix$df[] <- n - 1
    }
  } else {
    message("Confidence intervals are calculated based on correlation
            coefficient +/- 2 SE. This is generally not recommended!")
    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      cor.r - 2 * cor.se
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      cor.r + 2 * cor.se
    }
  }



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
        cor.df <- cor_matrix$df[i, j]
        cor.ci.low <- get_cor.ci.low(cor_matrix, cor.r, cor.se, i, j, cor.df)
        cor.ci.high <- get_cor.ci.high(cor_matrix, cor.r, cor.se, i, j, cor.df)
        output_cor[i, j] <- paste(.fmt_cor(cor.r), sigstars(cor.p, stars))
        output_ci[i, j] <- paste0(
          '<span style="font-size:80%">',
          "[", .fmt_cor(cor.ci.low), ", ", .fmt_cor(cor.ci.high), "]",
          "</span>"
        )
      }
    }
  }

  cor_cells <- paste(output_cor, output_ci, sep = "<br />")
  dim(cor_cells) <- dim(output_cor)

  if (is.null(extras)) {
    cells <- cbind(
      matrix(output_variable_names, ncol = 1),
      output_descriptives, cor_cells
    )
  } else {
    message("Note that the ordering of the 'extras' argument is not checked - ensure that it matches 'desc' in the correlation matrix.")
    cells <- cbind(
      matrix(output_variable_names, ncol = 1),
      output_descriptives, extras, cor_cells,
      stringsAsFactors = FALSE
    )
  }

  colnames(cells) <- c("Variable", "desc", colnames(extras), seq_len(length(output_variable_names) - 1))

  cells_df <- tibble::as_tibble(cells)

  tab <- cells_df %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::everything())

  if (apa_style) tab <- tab %>% gt_apa_style()

  notes %<>% c("*M* and *SD* are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation.")

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)
  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }

  if (is.character(add_title)) {
    tab <- tab %>% gt::tab_header(
      title = add_title
    )
  }

  tab <- tab %>% gt::cols_label(desc = gt::md("*M (SD)*"))
  #TK - add md formatting to extras column titles

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
#' for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included in this vector are retained.
#' @inheritParams psych::corr.test
#' @inheritDotParams psych::corr.test -y -minlength -ci
#' @return A list including the correlation matrix, p-values, standard errors, t-values, pairwise number of observations, confidence intervals and descriptives
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @export


cor_matrix <- function(x,
                       var_names = NULL,
                       method = c("pearson", "spearman", "kendall"),
                       adjust = "none",
                       ...) {

  x %<>% dplyr::select_if(is.numeric)

  if(!is.null(var_names)) x <- x[names(var_names)]

  # Compute correlation matrix
  correlation_matrix <- psych::corr.test(x, method = method[1], adjust = adjust, ...)
  cors <- correlation_matrix$r # Matrix of correlation coeficients
  p.values <- correlation_matrix$p # Matrix of p-value
  std.err <- correlation_matrix$se # Matrix of standard errors
  t.values <- correlation_matrix$t # Matrix of t-values
  n.matrix <- correlation_matrix$n # Matrix of pairwise counts

  # Copy (possibly) adjusted p-values into lower half that will be used by apa_cor_table()
  p.values[lower.tri(p.values)] <- t(p.values)[lower.tri(p.values)]

  #Ensure that n is a named matrix (corr.test returns single number for complete data)
  if(is.null(dim(n.matrix))){
    n.out <- n.matrix
    n.matrix <- cors
    n.matrix[TRUE] <- n.out
  }

  ci_low <- p.values
  ci_low[TRUE] <- NA
  ci_low[lower.tri(ci_low)] <- correlation_matrix$ci$lower

  ci_high <- p.values
  ci_high[TRUE] <- NA
  ci_high[lower.tri(ci_high)] <- correlation_matrix$ci$upper


  desc_stat <- x %>%
    psych::describe() %>%
    data.frame() %>%
    tibble::rownames_to_column("var") %>%
    dplyr::select(.data$var, M = .data$mean, SD = .data$sd)

  corM <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, n = n.matrix, ci.low = ci_low, ci.high = ci_high, desc = desc_stat)

  if (!is.null(var_names)) {
    corM[1:7] <- purrr::map(corM[1:7], function(x) {
      rownames(x) <- var_names[rownames(x)]
      colnames(x) <- var_names[colnames(x)]
      x
    })
    used_vars <- intersect(var_names, rownames(corM[[1]]))
    corM[1:7] <- purrr::map(corM[1:7], function(x) x[used_vars, used_vars])
    corM$desc$var <- var_names[corM$desc$var]
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
#' @examples
#' \dontrun{
#' if (requireNamespace("survey") & requireNamespace("srvyr")) {
#' library(survey)
#' library(srvyr)
#' data(api)
# Create survey design object
#' dstrat <- apistrat %>% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)
#'
#'var_names <- c(meals = "Share subsidized meals", ell = "English language learners",
#'               growth = "Performance Change")
#'
#' # Print correlation matrix
#' survey_cor_matrix(dstrat, var_names)
#' }
#' }

survey_cor_matrix <- function(svy_df, var_names) {
  .check_req_packages(c("jtools", "survey", "srvyr", "weights"))

  assert_class(svy_df, "survey.design")

  if(!inherits(svy_df, "tbl_svy")) svy_df %<>% srvyr::as_survey(svy_df)

  svy_df %<>%
    srvyr::select_if(is.numeric)

  if(!is.null(var_names))  svy_df %<>%
    srvyr::select(dplyr::one_of(names(var_names)))

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
  class(cor_matrix) <- "list"
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
        df <- rbind(df, data.frame(x = ii, y = jj, mice::pool(cor.ii.jj) %>% summary() %>% magrittr::extract(c("estimate", "p.value", "std.error", "statistic", "df")) %>% magrittr::extract(2, )))
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
  dfs <- to_matrix(df, variables, "df")

  imp_svy <- survey::svydesign(~1, weights = as.formula(paste0("~", dplyr::as_label(weights))), data = mitools::imputationList(mi_list))


  desc <- NULL
  for (i in 1:ct) {
    M <- mitools::MIcombine(with(imp_svy, survey::svymean(as.formula(paste0("~", variables[i])), design = .design)))[[1]]
    SD <- sqrt(mitools::MIcombine(with(imp_svy, survey::svyvar(as.formula(paste0("~", variables[i])), design = .design)))[[1]])
    desc <- rbind(desc, data.frame(var = variables[i], M = M, SD = SD))
  }

  corM <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, df = dfs, desc = desc, tests = df)

  if (!is.null(var_names)) {
    corM[1:5] <- purrr::map(corM[1:5], function(x) {
      rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
      colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
      x
    })
    used_vars <- intersect(var_names, rownames(corM[[1]]))
    corM[1:5] <- purrr::map(corM[1:5], function(x) x[used_vars, used_vars])
    rownames(corM$desc) <- rownames(corM$desc) %>% stringr::str_replace_all(var_names)
    corM$desc$var %<>% stringr::str_replace_all(var_names)
    corM$desc <- corM$desc[match(used_vars, corM$desc$var), ]
  }

  corM
}

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  lm(scale(y) ~ scale(x), weights = wt)
}

#' Create distribution charts to show in descriptive table
#'
#' Particularly in exploratory data analysis, it can be instructive to see histograms
#' or density charts.
#'
#' @param x A dataframe - if var_names is NULL, all numeric variables in x will be used, otherwise those included in var_names will be selected
#' @param var_names A named character vector with the names that should be displayed
#' for variables. If NULL, then the variables are not renamed. Particularly important
#' when output is to be combined with a correlation matrix, e.g., from \code{cor_matrix()}
#' @param plot_type Type of plot that should be produced - `histogram` or `density` plot. If `auto`,
#' histograms are produced for variables that take fewer than 10 unique values, density plots for others. If a number is provided,
#' that number is used as the maximum number of unique values for which a histogram is used.
#' @param hist_align_y Should histograms use the same y-axis, so that bin heights are comparable? Defaults to FALSE
#' @param plot_theme Additional theme_ commands to be added to each plot
#' @export
#' @examples
#' \dontrun{
#' plot_distributions(mtcars, var_names = c(wt = "Weight", mpg = "Efficiency",
#'                    am = "Transmission", gear = "Gears"))
#' }

plot_distributions <- function(x, var_names = NULL, plot_type = c("auto", "histogram", "density"), hist_align_y = FALSE, plot_theme = NULL) {
  x %<>% dplyr::select_if(is.numeric)

  if (!is.null(var_names)) x <- x[names(var_names)]

  plot_hist <- (
    if (is.numeric(plot_type)) {
      purrr::map_lgl(x, ~ (unique(.x) %>% length()) <= plot_type)
    } else {
      switch(plot_type[1],
        auto = purrr::map_lgl(x, ~ (unique(.x) %>% length()) < 10),
        histogram = rep(TRUE, ncol(x)),
        density = rep(FALSE, ncol(x)),
        stop('chart type needs to be one of "auto", "histogram" or "density" or a number')
      )
    })

  if (is.null(var_names)) var_names <- names(x)
  names(var_names) <- names(x)
  plots <- purrr::map2(names(var_names), plot_hist, function(var_name, plot_hist) {
    out <- ggplot2::ggplot(x, ggplot2::aes_string(var_name))
    if (plot_hist) {
      return(out + ggplot2::geom_histogram(bins = (x[[var_name]] %>% unique() %>% length()), col = "white", size = 3) + ggplot2::scale_x_continuous(breaks = (x[[var_name]] %>% unique())))
    }
    out + ggplot2::geom_density(fill = "grey", outline.type = "full")
  })

  if (hist_align_y) {
    ymax <- purrr::map_dbl(plots, ~ ggplot2::layer_scales(.x) %>%
      extract2("y") %>%
      extract2("range") %>%
      extract2("range") %>%
      extract(2))

    if (any(plot_hist)) {
      hist_max <- max(ymax[plot_hist])
      plots[plot_hist] <- purrr::map(plots[plot_hist], ~ .x + ggplot2::ylim(0, hist_max))
    }
  }
  plots <- purrr::map(plots, ~ .x + ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.line = ggplot2::element_blank()))
  if (!is.null(plot_theme)) plots <- purrr::map(plots, ~ .x + plot_theme)
  names(plots) <- var_names
  plots
}

#' Add plots into gt table column
#'
#' This function takes a list of ggplot2 plots and adds them into a gt table column.
#'
#' @param gt_table A gt table to add the plots into
#' @param plots A list of ggplot2 plots, typically with the same length as the number of rows in gt_table
#' @param col_index The index of the column in gt_table that is to be overwritten with the plots
#'
#' @export
#' @examples
#' \dontrun{
#' var_names <- c(wt = "Weight", am = "Transmission", mpg = "Consumption (mpg)", gear = "Gears")
#' cor_table <- cor_matrix(mtcars, var_names) %>%
#'   apa_cor_table(extras = tibble::tibble(Distributions = c(1:length(var_names))))
#' large_text <- ggplot2::theme(axis.text.x = ggplot2::element_text(size=40))
#' distr_plots <- plot_distributions(mtcars, var_names, plot_theme = large_text)
#' gt_add_plots(cor_table, distr_plots, 3)
#' }

gt_add_plots <- function(gt_table, plots, col_index) {
  purrr::walk(1:length(plots), function(x) {
    gt_table <<- gt::text_transform(gt_table, gt::cells_body(col_index,x), fn = function(y) {
      plots[[x]] %>%
        gt::ggplot_image(height = gt::px(50))
    })
  }
    )
  gt_table
}

