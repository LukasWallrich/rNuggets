

#' Significance stars for p-values
#'
#' Function returns significance stars for \emph{p}-values, most likely for use
#' in tables that report the results of multiple statistical tests.
#'
#' Symbols and tresholds are *** \emph{p} < .001,  ** \emph{p} < .01, * \emph{p}
#'  < .05 and † p < .1. The symbols can be changed by passing a 5-element
#'  character vector to the \code{stars} argument. For alignment, each should
#'  have the same length, they should indicate the signs for the four categories
#'  indicated above and for non-significant values (can of course be "   "). This
#'  argument makes most sense when marginal significance should not be indicated
#'  or when non-significant values should explicitly be labeled (e.g., as ns)
#'
#' @encoding UTF-8
#' @param p A \emph{p}-value or (more commonly) a vector of \emph{p}-values
#' @param stars A character vector to change the significance symbols (see details)
#' @return A character vector of significance stars for each \emph{p}-value,
#' each padded with spaces to be four characters long
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#'

sigstars <- function(p, stars = NULL) {
  if (is.null(stars)) stars = c("*** ", "**  ", "*    ", "\U2020    ", "    ")
  ifelse(p < .001, stars[1],
                                 ifelse(p < .01, stars[2],
                                        ifelse(p < .05, stars[3],
                                               ifelse(p < .1, stars[4], stars[5])
  )))
}

#' Calculates correlation matrix with significance stars and descriptives
#'
#' Calculates the correlation matrix between a given set of variables and
#' optionally includes a column with means and standard deviations of the
#' variables.
#'
#' Correlation stars are created with the \code{\link{sigstars}} function.
#' To change the symbols, the \code{stars} argument can be included here and will
#' be passed on.
#'
#' @param x Dataframe of variables that can be coerced to numeric.
#' @param remove_triangle Given that the upper and lower triangles of the
#' correlation matrix contain the same information, either should be dropped.
#' This parameter specific which.
#' @param desc_col Logical value that indicates whether a column with means
#' and standard deviations should be added in front of the correlations
#' @inheritParams psych::corr.test
#' @inheritDotParams sigstars stars
#' @return A dataframe including the correlation matrix
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#'

corstars <-
  function(x,
           method = c("pearson", "spearman", "kendall"),
           remove_triangle = c("upper", "lower"),
           desc_col = T,
           adjust = "none",
           ...) {

    #Compute correlation matrix
    x <- as.matrix(x)
    correlation_matrix <- psych::corr.test(x, method = method[1])
    r <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$p # Matrix of p-value

    ## Define notions for significance levels; spacing is important.
    mystars <- sigstars(p, ...)

    ## trunctuate the correlation matrix to two decimal
    r <- format(round(cbind(rep(-1.11, ncol(
      x
    )), r), 2))[, -1]

    ## build a new matrix that includes the correlations with their apropriate stars
    r_new <- matrix(paste(r, mystars, sep = ""), ncol = ncol(x))
    diag(r_new) <- paste(diag(r), " ", sep = "")
    rownames(r_new) <- colnames(x)
    colnames(r_new) <- paste(colnames(x), "", sep = "")
    ## remove upper triangle of correlation matrix
    if (remove_triangle[1] == "upper") {
      r_new <- as.matrix(r_new)
      r_new[!lower.tri(r_new, diag = TRUE)] <- ""
      r_new <- as.data.frame(r_new)
      r_new <- cbind(r_new[1:length(r_new) - 1])
    }

    ## remove lower triangle of correlation matrix
    else if (remove_triangle[1] == "lower") {
      r_new <- as.matrix(r_new)
      r_new[!upper.tri(r_new, diag = TRUE)] <- ""
      r_new <- as.data.frame(r_new)
      r_new <- cbind(r_new[1:nrow(r_new) - 1, 2:length(r_new)])
    }

    if(desc_col) {
      desc_stat <- x %>% psych::describe() %>% data.frame() %>%
        tibble::rownames_to_column("variable") %>% dplyr::mutate(
          M_SD = paste0(round(.data$mean,2), " (",round(.data$sd, 2), ")")) %>%
          dplyr::select(.data$variable, .data$M_SD)
      rownames(r_new) <- NULL
      return(cbind(desc_stat, r_new))
    }
    ## Bind descriptives and return the correlation matrix

    r_new

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
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
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

within_km <- function(df, start_longitude, start_latitude, km) {
  if(!all(c("latitude", "longitude") %in% colnames(df)))
    stop("Dataframe needs to contain `latitude`` and `longitude` variables")
  df %>% dplyr::select(.data$longitude, .data$latitude) %>%
    {purrr::map2(.data$longitude, .data$latitude, gcd.hf, start_longitude, start_latitude)}  %>%
    unlist() %>% {.data < km}
}
