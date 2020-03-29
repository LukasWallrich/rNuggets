apa.cor.table.survey <- function (cor.matrix, filename = NA, table.number = NA,
          landscape = TRUE)
{
  table_number <- table.number
  cor_matrix <- cor.matrix
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
    output_descriptives[i, 1] <- apaTables:::txt.number(cor_matrix$desc[i,2]) #Mean
    output_descriptives[i, 2] <- apaTables:::txt.number(cor_matrix$desc[i,3]) #SD
    for (j in 1:number_variables) {
      if ((j < i)) {
        cor.r <- cor_matrix$cors[i,j]
        cor.p <- cor_matrix$p.values[i,j]
        cor.se <- cor_matrix$std.err[i,j]
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
    table_note <- "Note. M and SD are used to represent mean and standard deviation, respectively.\nValues in square brackets indicate the 95% confidence interval.\nSignificance levels and confidence intervals are based on 1000 bootstrap resamples taking into account survey weights (Pasek, 2016).\n* indicates p < .05. ** indicates p < .01. *** indicates p < .001.\n"
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
      table_note <- "{\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation. Significance levels and confidence intervals are based on 1000 bootstrap resamples taking into account survey weights (Pasek, 2016). * indicates {\\i p} < .05. ** indicates {\\i p} < .01. *** indicates {\\i p} < .001."
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

survey_cor_matrix <- function(svy_df) {
  cor_matrix <- jtools::svycor(~., svy_df, na.rm = TRUE, sig.stats = TRUE)

  cor_matrix$desc <- svy_df %>% summarise_all(.funs=list(`1M` = survey_mean, `1SD`=survey_var), na.rm = TRUE) %>% select(!matches("_se")) %>% gather(key = "key", value = "value") %>%
    separate(key, into = c("var", "statistic"), sep = "_1") %>% spread(statistic, value) %>% mutate(SD = sqrt(SD)) %>% arrange(match(var, rownames(cor_matrix[[1]])))

  cor_matrix
  }
