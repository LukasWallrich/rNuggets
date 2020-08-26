
#' Significance stars for p-values
#'
#' Function returns significance stars for \emph{p}-values, most likely for use
#' in tables that report the results of multiple statistical tests. An empty
#' string is returned for NAs, unless that behaviour is overwritten.
#'
#'  Symbols and tresholds are *** \emph{p} < .001,  ** \emph{p} < .01, * \emph{p}
#'  < .05 and † \emph{p} < .1. The symbols can be changed by named character vector sorted
#'  descendingly to the \code{stars} argument. For the default, the argument would be
#' \code{stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)}
#'
#' @encoding UTF-8
#' @param p A \emph{p}-value or (more commonly) a vector of \emph{p}-values
#' @param stars A character vector to change the significance symbols (see details in `sigstars`)
#' @param ns Logical. Should non-significant values be highlighted as "ns"?
#' @param pad_html Should all results be padded right to the same width with HTML non-breaking spaces?
#' @param return_NAs Logical. Should NAs be returned? If not, empty strings are returned instead.
#' @return A character vector of significance stars for each \emph{p}-value,
#' each padded with spaces to be four characters long
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @export

sigstars <- function(p, stars = NULL, pad_html = FALSE, ns = FALSE, return_NAs = FALSE) {
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
  ns <- ifelse(ns == TRUE, "<sup>ns</sup>", "")
  if (pad_html) {
    .check_req_packages(c("xml2"), note = "Trying to add HTML-padding to sigstars.")
    stars2 <- names(stars)
    if (any(stringr::str_detect(names(stars), "&"))) stars2 <- purrr::map_chr(names(stars), .unescape_html)
    nchars <- purrr::map_int(stars2, nchar)
    len <- max(nchars)
    stars3 <- purrr::map_chr(stars2, .pad, len)
    stars3 %>% stringr::str_replace_all(stringr::fixed(stars2), names(stars))
    names(stars) = stars3
    ns <- paste0(ns, rep("&nbsp;", len-nchar(ns)), collapse = "")
  }

  out <- rep(ns, length(p))

  for (n in names(stars)) {
    out <- ifelse(p < stars[n], n, out)
  }

  if(!return_NAs) out[is.na(out)] <- ""

  out
}

.make_stars_note <- function (stars = NULL, markdown = TRUE)
{
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = .05, `**` = .01, `***` = .001)
  out <- stars
  if (markdown == TRUE) {
    out <- paste0(names(out), " *p* < ", sub('.', '', out))
  } else {
    out <- paste0(names(out), " p < ", sub('.', '', out))
  }
  out <- paste0(out, collapse = ", ")

  return(out)
}

.pad <- function(x, len, padding = "&nbsp;") {
  x<-as.character(x)
  n <- nchar(x)
  if(n<len) return(paste0(x, paste0(rep(padding, len-n), collapse = "")))
  x}

.unescape_html <- function(str){
  purrr::map_chr(str, function(x)
    xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>"))))
  }

#' Geosedic distance between two points.
#'
#' Calculates the geodesic distance between two points specified by their
#' longitude and latitude in degrees calculated using the Haversine formula.
#' This should give accurate results for small and medium distances, but assumes
#' a spherical earth. Algorithms that accurately model an ellipsoid earth are
#' slower, but should be used for long distances.
#'
#' @param long1 Longitude of point 1 in degrees.
#' @param lat1 Latitude of point 1 in degrees.
#' @param long2 Longitude of point 2 in degrees.
#' @param lat2 Latitude of point 2 in degrees.
#' @return The distance betwen the two points in kilometers.
#' @source Adapted from
#'   https://www.r-bloggers.com/great-circle-distance-calculations-in-r/


gcd.hf <- function(long1, lat1, long2, lat2) {
  long1 %<>% .deg2rad
  long2 %<>% .deg2rad
  lat1 %<>% .deg2rad
  lat2 %<>% .deg2rad
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  d <- R * c
  return(d) # Distance in km
}

.deg2rad <- function(deg) return(deg*pi/180)

std_stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)


#' Identify rows of dataframe with observations in certain distance from point
#'
#' \code{within_km} tests for each line of a dataframe with longitude and
#' latitude variables whether the location so described is within a certain
#' radius from a given starting point.
#'
#' Note that this function is very inefficient. It calculates the distance of
#' each line to the starting point using the Haversine formula. If you need to
#' use this regularly or on large datasets, a previous step to throw out lines
#' that are clearly too far away based on either latitude or longitude would
#' greatly increase efficiency.
#'
#' @param df Dataframe with observations, must include latitude and longitude
#' variables in degrees
#' @param start_latitude Latitude of starting point in degrees.
#' @param start_longitude Longitude of starting point in degrees.
#' @param km Distance from starting point to filter line by
#' @return Logical vector indicating which rows of \code{df} are within
#'  \code{km} from starting point
#'  @export

within_km <- function(df, start_longitude, start_latitude, km) {
  if (!all(c("latitude", "longitude") %in% colnames(df))) {
    stop("Dataframe needs to contain `latitude`` and `longitude` variables")
  }
  df %>%
    dplyr::select(.data$longitude, .data$latitude) %>%
    {purrr::map2(.data$longitude, .data$latitude, gcd.hf, start_longitude, start_latitude)} %>%
    unlist() %>%
    {.data < km}
}

#' Cut a continuous variable into given proportions
#'
#' \code{cut()} and similar functions can cut continous variables by quantile;
#' other helper functions exist to cut variables into groups of the same size
#' or width. This function cuts a contiuous variable into given proportions.
#'
#' Ties within the continuous variable are allocated randomly - so this function
#' should not be used if there are many ties. The number of observations per
#' group is rounded up for even-numbered levels (second, fourth, etc) and
#' rounded down for others (expect for the last level that is used to balance).
#' For large numbers of observations, the distribution will be very close to
#' what is desired, for very small numbers of observations, it should be checked.
#'
#' @param x A numeric variable that is to be cut into categories
#' @param p The proportion of cases to be allocated to each category, in
#' ascending order. Should add up to one, otherwise, it will be scaled accordingly
#' @param ties.method Currently accepts only "random" - could be expanded in the
#' future, though it is unclear what a better method would be
#' @param fct_levels Character vector with names for levels. If it is NULL, the
#' groups will be labeled with their number and the cut-points employed.
#' @return Factor variable with x cut into length(p) categories in given
#' proportions
#' @export

cut_p <- function(x, p, ties.method = "random", fct_levels = NULL) {
  if (!ties.method == "random") stop('Currently, only "random" is accepted as ties.method.', call. = FALSE)
  if (sum(p) != 1) {
    message("p should be probabilities that add up to 1 - will be scaled accordingly")
    p <- p / sum(p)
  }

  xNA <- x
  x <- x[!is.na(x)]

  ranks <- rank(x, na.last = "keep", ties.method)
  start <- min(x)
  end <- x[match(.floor_ceiling(p[1] * length(x), 1), ranks)]
  out <- rep(paste0("Group ", 1, " (", start, " to ", end, ")"), ceiling(p[1] * length(x)))
  for (i in seq.int(2, length(p) - 1, 1)) {
    start <- x[match(.floor_ceiling(cumsum(p)[i - 1] * length(x) + 1, i - 1), ranks)]
    end <- x[match(.floor_ceiling(cumsum(p)[i] * length(x), i), ranks)]
    out <- c(out, rep(paste0("Group ", i, " (", start, " to ", end, ")"), .floor_ceiling(p[i] * length(x), i)))
  }
  start <- x[match(.floor_ceiling(cumsum(p)[length(p) - 1] * length(x) + 1, length(p) - 1), ranks)]
  end <- max(x)
  out <- c(out, rep(paste0("Group ", length(p), " (", start, " to ", end, ")"), length(x) - length(out)))

  out <- factor(out)

  if (!is.null(fct_levels)) {
    if (!length(fct_levels) == length(p))
      stop("Arguments fct_levels and p need to have same length", call. = FALSE)
    levels(out) <- fct_levels
  }

  xNA[!is.na(xNA)] <- out[ranks]
  xNA <- factor(xNA)
  if (!is.null(fct_levels)) {
    if (!length(fct_levels) == length(p))
      stop("Arguments fct_levels and p need to have same length", call. = FALSE)
    levels(xNA) <- fct_levels
  }
  xNA
}

#' Helper function to round up and down in turn
#'
#' Iterates between floor() and ceiling()
#'
#' @param x Numeric, to be rounded
#' @param i Iterator. floor() will be used on x for odd i, ceiling() for even i

.floor_ceiling <- function(x, i) {
  if (i %% 2 == 1) return(ceiling(x))
  floor(x)
}

#' Formats a number to print as percentage
#'
#'Takes a number and returns it as a formatted string expressing the percentage
#'
#' @param x A number
#' @param digits The number of digits after the percentage point. Defaults to 1
#' @export

p_pct <- function(x, digits = 1) {
  paste0(format(round(x*100, digits), nsmall = 2), "%")
}


#' Round all numeric columns in dataframe
#'
#' Rounds all numeric columns in dataframe, using the R default "half to even"
#'
#' @param df Dataframe to be rounded
#' @param digits Number of digits, defaults to 2
#' @source https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
#' @export


round_df <- function(df, digits = 2) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] <- round(df[, nums], digits = digits)
  (df)
}

#'Scales a vector and returns it without attributes
#'
#'The `base::scale()` function adds attributes to the output that can lead to
#'problems later on. This function scales a vector and strips the attributes.
#'
#' @inheritParams base::scale
#' @export

scale_blank <- function(x, center = TRUE, scale = TRUE) {
  as.numeric(scale(x))
}



#'Format p-value in line with APA standard (no leading 0)
#'
#'Formats p-value in line with APA standard, returning it without leading 0 and
#'as < .001 when it is that small.
#'
#' @param p_value Numeric, or a vector of numbers
#' @param sig_dig Number of signficant digits, defaults to 3
#' @export

fmt_p <- function(p_value, sig_dig = 3) {
  fmt <- paste0("%.", sig_dig, "f")
  fmt_p <- function(x) paste0("= ", sprintf(fmt, x)) %>%
  stringr::str_replace(" 0.", " .")
  exact <- ifelse(p_value < .001, FALSE, TRUE)
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  out[!exact] <- "< .001"
  out
}

.fmt_pct <- function(x, digits = 1) {
  fmt <- paste0("%1.", digits, "f%%")
  sprintf(fmt, x*100)
}


.fmt_cor <- function(cor_value, sig_dig = 2) {
  fmt <- paste0("%.", sig_dig, "f")
  sprintf(fmt, cor_value) %>%
    stringr::str_replace("0.", ".")
}

#'Converts a tibble to tribble code
#'
#'Tribbles are an easy way to legibly input data, and therefore helpful for teaching and interactive work. This function takes
#'a tibble and returns code that can recreate it. Note that this function converts
#'"NA" to NA.
#'
#' @param x The tibble to be converted into tribble code
#' @param show Logical. Print code (otherwise, returned - print with `cat()` to get linebreaks etc)
#' @export
#'
## TODO:
## Use padding to show tribble code with aligned columns

to_tribble <- function(x, show = FALSE) {
  no_cols <- ncol(x)
  #lengths <- purrr::map_int(x, ~max(nchar(.x)))
  #if(sum(lengths)+no_cols*3) > 80) message("Some entries are too long for the tibble code to be well formatted")
  vars <- names(x)
  x %<>% dplyr::mutate_if(is.character, function(x) paste0('"', x, '"'))

  code <- "tibble::tribble(
  "

  for (i in seq_len(length(vars))) {
    code %<>% paste0("~", vars[i], ", ")
  }
  code %<>% paste0("\n")
  for (j in seq_len(nrow(x))) {
    for (i in seq_len(length(vars))) {
      code %<>% paste0(x[j, i], ", ")
    }
    code %<>% paste0("\n  ")
  }

  code %<>% stringr::str_replace_all('"NA"', "NA")

  code %<>% substr(1, nchar(.)-2) %>% paste0("\n)\n")
  if (show) {
    cat(code)
    return(invisible(code))
  }
  code
}

#' Collapse factor levels into "Other"
#'
#' This function duplicates \code{forcats::fct_other}. It is therefore
#' deprecated and should not be used. At present, I need it for compatibility.
#' It will be removed in the future.
#'
#'@param large_factor The existing factor
#'@param cats The levels to keep
#'@param other The name of the new "other"-level
#'@export

simplify_factor <- function(large_factor, cats, other = "Other") {
  .Deprecated("fct_other in the forcats package")
  cats %<>% c(NA)
  levels(large_factor) <- c(levels(large_factor), other)
  large_factor[!(large_factor %in% cats)] <- other
  droplevels(large_factor)
}

#' Show group counts and group means in srvyr data
#'
#' This function groups srvyr data by a grouping variable and then calculates
#' and displays group means and counts with standard errors.
#'
#' @param df A srvyr survey object
#' @param gr Character. The name of the grouping variable in df.
#' @param mean_vars Character vector. Names of one or more variables in df to calculate means for.
#' @param tbl_title Character. Title for summary table to be printed.
#' @param quietly Logical. Calculate means without displaying them?
#' @return Dataframe with group counts and means
#' @export

svy_group_means <- function(df, gr, mean_vars, tbl_title, quietly = T) {
  .check_req_packages("survey")

  cmd <- paste(purrr::map(mean_vars, function(x) paste0("Mean_", x, " = survey_mean(", x, ")")),
               collapse = ", ")
  means <- eval(parse(text = paste0("df %>% srvyr::group_by(", gr, ") %>% summarize(N = survey_total(na.rm=T), ",
                                    cmd, ")")))
  if (!quietly)
    means %>% knitr::kable(caption = tbl_title, digits = 2) %>%
    kableExtra::kable_styling(full_width = F, position = "left") %>%
    print()
  return(means)
}

#' Show group counts and group means in multiply imputed and weighted data
#'
#' This function groups srvyr data by a grouping variable and then calculates
#' and displays group means and counts with standard errors.
#'
#' @param mi_list A list of dataframes of multiple imputation results
#' @param mean_var Variable in mi_list to calculate means for.
#' @param gr Grouping variable in mi_list
#' @param weights Variable within mi_list that gives the survey weights
#'
#' @return A tiblle with means (M), standard deviations (SD) and weighted counts (N) per group
#' @export

 wtd_group_means_mi <- function (mi_list, mean_var, gr, weights)
 {
   if ("quosure" %in% class(weights)) {
     fmla_weights <- as.formula(paste0("~`", dplyr::as_label(weights),
                                       "`"))
     fmla_gr <- as.formula(paste0("~`", dplyr::as_label(gr), "`"))
     fmla_mean_var <- as.formula(paste0("~`", dplyr::as_label(mean_var),
                                        "`"))
   }
   else {
     fmla_weights <- as.formula(paste0("~`", substitute(weights),
                                       "`"))
     fmla_gr <- as.formula(paste0("~`", substitute(gr), "`"))
     fmla_mean_var <- as.formula(paste0("~`", substitute(mean_var),
                                        "`"))
   }
   imp_svy <- survey::svydesign(~1, weights = fmla_weights,
                                data = mitools::imputationList(mi_list))
   M <- mitools::MIcombine(with(imp_svy, survey::svyby(fmla_mean_var,
                                                       fmla_gr, design = .design, FUN = survey::svymean)))
   VAR <- mitools::MIcombine(with(imp_svy, survey::svyby(fmla_mean_var,
                                                         fmla_gr, design = .design, FUN = survey::svyvar)))
   TOT <- mitools::MIcombine(with(imp_svy, survey::svyby(fmla_mean_var,
                                                         fmla_gr, design = .design, FUN = survey::svytotal)))
   out <- tibble::tibble(level = names(M$coefficients), M = M$coefficients,
                         SD = sqrt(VAR$coefficients), N = TOT$coefficients/.data$M,
                         group_var = dplyr::as_label(rlang::enquo(gr)))
 }

 #' Get code to generate tibbles to rename categorical variables and their levels
 #'
 #' Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
 #' function generates code in which only the new names need to be modified, and which can then be passed
 #' to either \code{\link{rename_cat_variables}} or directly to \code{\link{cat_var_table_mi}}
 #'
 #' Only categorical variables should be passed to the function if code for levels is
 #' requested. If a variable has more than 20 distinct values, it is dropped from the levels-tribble-code
 #'
 #'
 #' @param dat A dataframe that contains the variables - only used to extract their possible levels.
 #' @param ... The variables to be included in the rename tribbles.
 #' @param show Logical - should the output be printed to the console. In any case, it is returned invisibly
 #' @param which Should tribble code be generated for variables (\code{"vars"}), levels (\code{"levels"}) or both (\code{"both"}) (default)
 #' @param max_levels The maximum number of levels before a variable is dropped from the levels_tribble. Defaults to 20
 #'
 #' @return Code to be edited and passed to tibble::tribble() to create var_names and level_names arguments for
 #' \code{\link{rename_cat_variables}} and \code{\link{cat_var_table_mi}}
 #'
 #' @export

get_rename_tribbles <- function(dat, ..., show = TRUE, which = c("both", "vars", "levels"), max_levels = 20) {
  vars <- rlang::enquos(...)
  vars_chr <- purrr::map_chr(vars, dplyr::as_label)
  out <- list()
  if (which[1] %in% c("both", "vars")) {
    vars_tribble <- tibble::tibble(old = vars_chr, new = vars_chr) %>% to_tribble(show = show)
    out <- c(out, vars_tribble)
  }
  if (which[1] %in% c("both", "levels")) {
    get_levels <- function(x, dat) {
      dat %>%
        dplyr::select(!!x) %>%
        dplyr::pull() %>%
        factor() %>%
        levels()
    }
    levels_list <- purrr::map(vars, get_levels, dat)
    levels_list <- Filter(function(x) length(x)<=max_levels, levels_list)
    if(length(levels_list)>=1) {
        names(levels_list) <- vars_chr

    mt <- function(x, name) {
      tibble::tibble(var = name, level_old = x, level_new = x)
    }
    levels_tribble <- purrr::lmap(levels_list, function(x) purrr::map(x, mt, names(x))) %>%
      purrr::map_dfr(rbind) %>%
      tibble::as_tibble() %>%
      to_tribble(show = show)
    out <- c(out, levels_tribble)
  }}

  out
}


#' Rename variables and/or their levels
#'
#' Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
#' function accepts tibbles containing the old and new names for arguments and levels, and returns a dataframe
#' (or list of dataframes, if one is passed) with variables and levels renamed.
#'
#'
#' @param dat A dataframe or list of dataframes (e.g., from multiple imputation) contains the variables. If a list is passed, it must have class "list"
#' @param ... The variables to be renamed
#' @param var_names A tibble containing `old` and `new` names for the variables. If NULL, only levels are renamed.
#' @param level_names A tibble containing old `var` names and `level_old` and `level_new` names. If NULL, only variables are renamed.
#'
#' @return The dataframe or list of dataframes passed to dat, with variables and/or levels renamed
#' @export

rename_cat_variables <- function(dat, ..., var_names = NULL, level_names = NULL) {
  if (!"list" %in% class(dat)) dat <- list(dat)
  vars <- rlang::enquos(...)

  if (!is.null(level_names)) {
    level_names_lst <- split(level_names, level_names$var)

    relevel <- function(dat, var, levels_old, levels_new) {
      var <- var[1]
      names(levels_old) <- levels_new
      dat <- dat %>% dplyr::mutate(!!var := forcats::fct_recode(!!rlang::sym(var), !!!levels_old))
      dat
    }

    for (i in seq_along(level_names_lst)) {
      dat <- purrr::map(dat, relevel, level_names_lst[[i]]$var, level_names_lst[[i]]$level_old, level_names_lst[[i]]$level_new)
    }
  }

  if (!is.null(var_names)) {
    var_names_chr <- var_names$old
    names(var_names_chr) <- var_names$new
    dat <- purrr::map(dat, dplyr::rename, !!!var_names_chr)
  }

  if (length(dat) == 1) dat <- dat[[1]] # To return data.frame if dataframe was passed
  dat
}


#' Add class
#'
#' This function adds a given class to an object, so that
#' different S3 methods can be called (e.g., `tidy.exp` to get OR for logistic regression models
#' (e.g., in `modelsummary::msummary`)
#'
#'
#' @param x An object
#' @param class_to_add String of the class to add, defaults to "exp"


add_class <- function(x, class_to_add = "exp") {
  class(x) <- c(class_to_add, class(x))
  x
}

#' Tidy function to exponentiate coefficients
#'
#' This function calls the tidy method based on the second class of the
#' object (i.e. after removing the "exp" class that led to it being called),
#' and then exponentiates the returned estimates and confidence intervals (if any)
#' in the tibble. This is usually used to turn coefficients of logistic
#' regression models into Odds Ratios.
#'
#'
#' @param x An object, usually containing a logistic regression model. Should
#' have the class "exp" as the first of its classes, and then a class that dispatches
#' it to an appropriate `generics::tidy`` function
#' @param ... Arguments passed on to the appropriate `tidy` function
#' @export

tidy.exp <- function(x, ...) {
  class(x) <- class(x)[-1]
  out <- generics::tidy(x, ...)
  out$estimate <- exp(out$estimate)
  if(!is.null(out$conf.high)) {
    out$conf.high <- exp(out$conf.high)
    out$conf.low <- exp(out$conf.low)
  }
  out
}

#' Tidy polr with added p-values
#'
#' This function calls tidy.polr and then adds a p.value column based
#' on the `MASS::dropterm` chi-squared test. This approach to testing
#' the significance of model terms was recommeded by Prof Brian Ripley,
#' the author of the MASS package.
#'
#'
#' @param x An object containing a `polr` model.
#' @param ... Arguments passed on to the `tidy.polr` function
#' @source https://r.789695.n4.nabble.com/p-values-of-plor-td4668100.html
#' @export

tidy.polr_p <- function(x, ...) {
  class(x)[1] <- "polr"
  out <- generics::tidy(x, ...)
  sig <- MASS::dropterm(x, test = "Chisq")
  p <- sig %>% dplyr::select(.data$`Pr(Chi)`) %>% dplyr::pull() %>% .[-1]

  terms <- purrr::map(rownames(sig)[-1], function(x) out$term[stringr::str_detect(out$term, stringr::fixed(x))]) %>% unlist()
  out <- dplyr::left_join(out, tibble::tibble(term = terms, p.value = p), by = "term")
  out
}

#' Copy data to clipboard to paste into Excel
#'
#' This function copies a dataframe into the clipboard, so that it can be
#' pasted into excel.
#'
#' @param df Dataframe to be copied.
#' @param row_names Logical. Should row names be copied?
#' @param col_names Logical. Should column names be copied?
#' @param ... Further arguments passed to `write.table`
#' @source https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/


clip_excel <- function(df,row_names=FALSE,col_names=TRUE,...) {
  utils::write.table(df,"clipboard",sep="\t",row.names=row_names,col.names=col_names,...)
}


#' Dump objects to clipboard (to transfer them between R sessions)
#'
#' This function calls dump on one or several R objects, which creates code that recreates them from the console.
#' It then copies this code to the clipboard. This can be used to quickly copy (small) objects between R sessions,
#' for instance during package development and testing, or - of course - to paste the dump code into a forum post.
#'
#' @param objects A character vector containing the names of one or more objects in the current session.
#' @export

dump_to_clip <- function(objects) {
  .check_req_packages("clipr")
  if(!is.character(objects)) stop("'objects' need to be a character vector with one or more R objects")
  utils::capture.output(dump(objects, file="")) %>% clipr::write_clip()
}


.fmt_ci <- function(lower, upper, digits = 2) {
  paste0("[", sprintf(paste0("%.", digits, "f"), round(lower, digits)), ", ", sprintf(paste0("%.", digits, "f"), round(upper, digits)), "]")
}


