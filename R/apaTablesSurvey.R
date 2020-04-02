#' Create a correlation table from survey data with summary statistics in APA style
#'
#' This function, based on apaTables' \code{apa.cor.table()}, creates (and
#' optionally saves) a correlation table with summary statistics.
#'
#' @param cor_matrix A survey correlation matrix, usually returned from \code{
#' survey_cor_matrix()}
#' @param note Additional notes to show under the table.
#' @param table_number Integer to use in table number output line
#' @inheritParams apaTables::apa.cor.table
#' @source Based on the apaTables \code{apa.cor.table()} function, but adapted to
#' accept survey correlation matrix
#' @return A table that can be printed to the console. The function is typically
#' more useful for saving a table to file, using the respective argument.
#'
apa_cor_table_survey <- function (cor_matrix, filename = NA, table_number = NA,
          landscape = TRUE, note = "") {
  req_packages <- c("apaTables", "jtools", "survey", "srvyr")
  if (suppressWarnings(!all(lapply(req_packages, requireNamespace, quietly=TRUE)))) {
    stop(paste0("Some required packages are not installed. Make sure you have
               these packages: ", paste0(req_packages, collapse = ", ")),
         call. = FALSE)
  }

  if (is.na(filename)) {
    make_file_flag <- FALSE
  }
  else {
    make_file_flag <- TRUE
  }
  df_col <- dim(cor_matrix[[1]])[2]
  number_variables <- df_col
  number_columns <- df_col - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_cor_rtf <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_ci_rtf <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(" ", number_variables,
                                2)
  output_variable_names <- paste(as.character(1:number_variables),
                                 ". ", rownames(cor_matrix[[1]]), sep = "")
  for (i in 1:number_variables) {
    output_descriptives[i, 1] <- apaTables:::txt.number(cor_matrix$desc[i, 2]) #Mean
    output_descriptives[i, 2] <- apaTables:::txt.number(cor_matrix$desc[i, 3]) #SD
    for (j in 1:number_variables) {
      if ((j < i)) {
        cor.r <- cor_matrix$cors[i, j]
        cor.p <- cor_matrix$p.values[i, j]
        cor.se <- cor_matrix$std.err[i, j]
        cor_string <- paste(apaTables:::strip.leading.zero(sprintf("%1.2f",
                                                  cor.r)), sigstars(cor.p))
        output_cor[i, j] <- cor_string
        output_cor_rtf[i, j] <- cor_string
        cor_ci_string <- apaTables:::txt.ci.brackets(cor.r-2*cor.se, cor.r+2*cor.se)
        output_ci[i, j] <- cor_ci_string
        output_ci_rtf[i, j] <- paste("{\\fs20",
                                     cor_ci_string, "}", sep = "")
      }
    }
  }
  left_padding <- c(" ", " ", " ")
  first_line <- c(output_variable_names[1], output_descriptives[1,
                                                                ], output_cor[1, ])
  first_line_rtf <- c(output_variable_names[1], output_descriptives[1,
                                                                    ], output_cor_rtf[1, ])
  second_line <- c(left_padding, output_ci[1, ])
  second_line_rtf <- c(left_padding, output_ci_rtf[1, ])
  third_line <- rep(" ", length(second_line))
  output_matrix_console <- rbind(first_line, second_line)
  output_matrix_rtf <- rbind(first_line_rtf, second_line_rtf)
  for (i in 2:number_variables) {
    first_line <- c(output_variable_names[i], output_descriptives[i,
                                                                  ], output_cor[i, ])
    first_line_rtf <- c(output_variable_names[i], output_descriptives[i,
                                                                      ], output_cor_rtf[i, ])
    second_line <- c(left_padding, output_ci[i, ])
    second_line_rtf <- c(left_padding, output_ci_rtf[i, ])
    third_line <- rep(" ", length(second_line))
      new_lines <- rbind(first_line, second_line, third_line)
      new_lines <- rbind(first_line, second_line, third_line)
      new_lines_rtf <- rbind(first_line_rtf, second_line_rtf,
                             third_line)
    output_matrix_console <- rbind(output_matrix_console,
                                   new_lines)
    output_matrix_rtf <- rbind(output_matrix_rtf, new_lines_rtf)
  }
  rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
  colnames(output_matrix_console) <- c(c("Variable",
                                         "M", "SD"), as.character(1:number_columns))
  rownames(output_matrix_rtf) <- rownames(output_matrix_console)
  colnames(output_matrix_rtf) <- colnames(output_matrix_console)

    table_title <- "Means, standard deviations, and correlations with confidence intervals\n"

  row_with_colnames <- colnames(output_matrix_console)
  df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
  rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
  table_body <- output_matrix_console
  if("svycor" %in% class(cor_matrix) & note == "") note <- "Significance levels and confidence intervals are based on 1000 bootstrap resamples taking into account survey weights (Pasek, 2016).\n"
    table_note <- paste("Note. M and SD are used to represent mean and standard deviation, respectively.", "Values in square brackets indicate the 95% confidence interval.",note, "* indicates p < .05. ** indicates p < .01. *** indicates p < .001.\n", sep = "\n")
  tbl.console <- list(table.number = table_number, table.title = table_title,
                      table.body = table_body, table.note = table_note)
  class(tbl.console) <- "apa.table"
  if (make_file_flag == TRUE) {
    colnames(output_matrix_rtf) <- c(c("Variable",
                                       "{\\i M}", "{\\i SD}"), as.character(1:number_columns))
    number_columns <- dim(output_matrix_rtf)[2]
    blankLine <- rep("", number_columns)
    output_matrix_rtf <- rbind(blankLine, output_matrix_rtf)
      table_title <- "Means, standard deviations, and correlations with confidence intervals"
      table_note <- paste0("Note. M and SD are used to represent mean and standard deviation, respectively.", "Values in square brackets indicate the 95% confidence interval.",note, "* indicates p < .05. ** indicates p < .01. *** indicates p < .001.\n", sep = "\n")

      table_note <- paste("{\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation.", note, " * indicates {\\i p} < .05. ** indicates {\\i p} < .01. *** indicates {\\i p} < .001.")
    rtfTable <- apaTables:::RtfTable$new(isHeaderRow = TRUE)
    rtfTable$setTableContent(output_matrix_rtf)
    rtfTable$setRowFirstColumnJustification("left")
    txt_body <- rtfTable$getTableAsRTF(FALSE, FALSE)
    apaTables:::write.rtf.table(filename = filename, txt.body = txt_body,
                    table.title = table_title, table.note = table_note,
                    landscape = landscape, table.number = table_number)
  }
  return(tbl.console)
}

#' Create a correlation matrix from survey data with summary statistics
#'
#' This function wraps jtools::svycor() so that it works in a srvyr-pipeline,
#' runs bootstrapped significance-tests and calculates weighted summary
#' statistics. Only numeric variables are included in the result.
#'
#' @param svy_df A survey object created with the survey or srvyr package. Only
#' numeric variables will be included in the result.
#' @return A correlation matrix list in the format provided by
#' \code{jtools::svycor()} with the addition of a \code{desc}-element with means
#' and standard deviations of the variables.
#'
#'

survey_cor_matrix <- function(svy_df) {
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
#' @return A correlation matrix list similar to the format provided by
#' \code{jtools::svycor()} with the addition of a \code{desc}-element with means
#' and standard deviations of the variables.
#' @source Takes some code from the \code{miceadds::micombine.cor} function,
#' but adapted to use weights and return in the format accepted by
#' \code{apa.cor.table.survey}
#'

wtd_cor_matrix_mi <- function(mi_list, weights) {
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

  list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, desc = desc, tests=df)

}

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  lm(scale(y)~scale(x), weights = wt)
}
