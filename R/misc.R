

#' Significance stars for p-values
#'
#' Function returns significance stars for \emph{p}-values, most likely for use
#' in tables that report the results of multiple statistical tests.
#'
#'  Symbols and tresholds are *** \emph{p} < .001,  ** \emph{p} < .01, * \emph{p}
#'  < .05 and † \emph{p} < .1. The symbols can be changed by named character vector sorted
#'  descendingly to the \code{stars} argument. For the default, the argument would be
#' \code{stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)}
#'
#' @encoding UTF-8
#' @param p A \emph{p}-value or (more commonly) a vector of \emph{p}-values
#' @param stars A character vector to change the significance symbols (see details in `sigstars`)
#' @param pad_html Should all results be padded right to the same width with HTML non-breaking spaces?
#' @return A character vector of significance stars for each \emph{p}-value,
#' each padded with spaces to be four characters long
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @export

sigstars <- function(p, stars = NULL, pad_html = FALSE) {
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
  ns <- ""
  if (pad_html) {
    .check_req_packages(c("xml2"), note = "Trying to add HTML-padding to sigstars.")
    stars2 <- names(stars)
    if (any(stringr::str_detect(names(stars), "&"))) stars2 <- purrr::map_chr(names(stars), .unescape_html)
    nchars <- purrr::map_int(stars2, nchar)
    len <- max(nchars)
    stars3 <- purrr::map_chr(stars2, .pad, len)
    stars3 %>% stringr::str_replace_all(stringr::fixed(stars2), names(stars))
    names(stars) = stars3
    ns <- paste0(rep("&nbsp;", len), collapse = "")
  }

  out <- rep(ns, length(p))

  for (n in names(stars)) {
    out <- ifelse(p < stars[n], n, out)
  }
  out
}

.make_stars_note <- function (stars = NULL)
{
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
  out <- stars
  out <- paste0(names(out), " p < ", out)
  out <- paste0(out, collapse = ", ")

  return(out)
}

.pad <- function(x, len, padding = "&nbsp;") {
  x<-as.character(x)
  n <- nchar(x)
  if(n<len) return(paste0(x, paste0(rep(padding, len-n), collapse = "")))
  x}

.unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
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
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  out[!exact] <- "< .001"
  out
}

.fmt_cor <- function(cor_value, sig_dig = 2) {
  fmt <- paste0("%.", sig_dig, "f")
  sprintf(fmt, cor_value) %>%
    stringr::str_replace("0.", ".")
}

#'Converts a tibble to tribble code
#'
#'Tribbles are an easy way to legibly input data, and therefore helpful for teaching and interactive work. This function takes
#'a tibble and returns code that can recreate it.
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

  code <- "tribble(
  "

  for (i in seq_len(length(vars))) {
    code %<>% paste0("~", vars[i], ", ")
  }
  code %<>% paste0("\n\n")
  for (j in seq_len(nrow(x))) {
    for (i in seq_len(length(vars))) {
      code %<>% paste0(x[j, i], ", ")
    }
    code %<>% paste0("\n")
  }
  code %<>% substr(1, nchar(.data)-2) %>% paste0("\n)")
  if (show) cat(code)
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

svy_group_means <- function(df, gr, mean_vars, tbl_title, quietly = F) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package \"survey\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
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
