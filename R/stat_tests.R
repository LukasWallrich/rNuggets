#' Pairwise t.test with Cohen's d
#'
#' This function takes two variables that are representing paired data and
#' calculates a paired samples \code{t.test}. It then also calculates and prints
#' Cohen's d as a measure of effect size and shows a clearer data label than
#' the t.test function.
#'
#' @param df A dataframe
#' @param x,y Character strings indicating the names of the two variables
#' @return Invisibly returns a list including the t.test() output and Cohen's D
#' @export

paired_t_test_d <- function(df, x, y) {
  t.test_result <- t.test(x = df[[x]], y = df[[y]], paired = T)
  t.test_result$data.name <- paste(x, "vs.", y)
  print(t.test_result)
  cohens_d <- t.test_result$estimate /
    (sqrt(t.test_result$parameter + 1) * t.test_result$stderr)
  print(paste("Cohen's d:", round_(cohens_d, 3)))
  invisible(list(t.test_result, cohens_d))
}

#' t.test for survey object with Cohen's d
#'
#' This function calculates a t.test() for two groups in a \code{srvyr} survey
#' object. It is particularly helpful when the grouping variable has more than
#' two levels and you just want to compare two of them.
#'
#' @param df A dataframe
#' @param dv Character. Name of the dependent variable for the t.test (numeric)
#' @param iv Character. Name of the grouping variable for the t.test (factor)
#' @param pair Character vector of length 2. Levels of iv to
#'    be compared in t.test
#' @param ttest Logical. Should t.test be run and displayed? Otherwise, only
#'   Cohen's d is calculated. Defaults to TRUE.
#' @return Invisibly returns a list including the t.test() output and
#'   Cohen's d
#' @export

svy_cohen_d_pair <- function(df, dv, iv, pair, ttest = T) {
  df <- eval(parse(text = paste0(
    "update(df, filt = factor(df$variables$",
    iv, ", levels = c('", paste0(pair,
      collapse = "', '"
    ), "')))"
  )))
  t.test_result <- NULL
  if (ttest) {
    print(paste0("T-Test for ", paste0(pair, collapse = " & "), ":"))
    t.test_result <- eval(parse(text = paste0(
      "survey::svyttest(", dv, " ~ ", iv,
      ", subset(df, !is.na(filt)))"
    )))
    print(t.test_result)
  }
  # Calculate Cohen's d
  means <- survey::svyby(~ get(dv), ~filt, df, survey::svymean)[1:2]
  names(means) <- c(dv, "mean")
  vars <- survey::svyby(~ get(dv), ~filt, df, survey::svyvar)[1:2]
  names(vars) <- c(dv, "var")
  cohens_d <- (means[1, 2] - means[2, 2]) / sqrt((vars[1, 2] + vars[2, 2]) / 2)
  print(paste0(
    "Cohen's d for pair ", paste0(pair, collapse = " & "), " is:",
    round_(cohens_d, 3)
  ))
  invisible(list(t.test_result, cohens_d))
}

#' Pairwise t.tests with effect sizes and survey weights
#'
#' This function calculates a t.test() for any pair of levels in a
#' \code{srvyr} survey object. It does currently not do any p-value adjustment
#' for multiple comparisons, and print rather than returns the results.
#'
#' @param cats Character vector of factor levels to be included in the
#' pairwise tests. If set to NULL, all levels are used.
#' @inheritDotParams svy_cohen_d_pair -pair
#' @inheritParams svy_cohen_d_pair
#' @return Invisibly returns a names lists of lists including the t.test()
#'   output and Cohen's D for each pair
#' @export

## ToDos:
### allow this to run without printing all results
### improve return with broom

svy_pairwise.t.test <- function(df, dv, iv, cats, ...) {
  if (is.null(cats)) {
    cats <- eval(parse(text = paste0("levelsdf$variables$", iv)))
  }

  print(paste(
    "Beware - NO correction for multiple comparisons is employed.\n",
    factorial(length(cats)) / (factorial(length(cats) - 2) * 2),
    "pairwise tests will be performed."
  ))
  df2 <- purrr::cross_df(data.frame(cats, cats, stringsAsFactors = F),
    .filter =
      function(x, y) as.character(x) <= as.character(y)
  )
  x <- purrr::map(purrr::pmap(df2, c), function(x) svy_cohen_d_pair(pair = x, ...))

  names(x) <- tidyr::unite(df2, .data$new_col)[[1]]
  x
}

#' lm() with standardised continuous variables
#'
#' This runs lm() after standardising all continuous variables, while leaving
#' factors intact.
#'
#' In the model call, the weights variable will always be called weights. This might
#' pose a problem when you update the model later on, for  the moment the only workaround
#' is to rename the weights variable accordingly (or to fix it and contribute a PR on
#' Github).
#'
#' @inheritParams stats::lm
#' @param rename_std Logical. Should standardised variables be indicated by _sd
#' suffix
#' @inheritDotParams stats::lm -data
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. If you want to run a model with all variables standardised,
#' one option is `QuantPsyc::lm.beta()`
#' @export

lm_std <- function(formula, data = NULL, weights = NULL, rename_std = FALSE, ...) {

  if (any(stringr::str_detect(as.character(formula), "factor\\("))) stop("Functions in the formula are applied after standardising - thus factor() needs to be used before lm_std() is called")

  parent <- parent.frame()
  here <- environment()
  vars <- all.vars(formula)

  if (!is.null(data)) {
    attach(data, name = "rN_lm_std_df")
    on.exit(detach("rN_lm_std_df"))
  }
  vars_num <- vars[purrr::map_lgl(vars, ~ is.numeric(get(.x, parent)))]

  vars_dummies <- vars_num[purrr::map_lgl(vars_num, ~ dplyr::n_distinct(get(.x, parent)) < 3)]

  if(length(vars_dummies)>0) warning("The following variables have less than three distinct values but are of type numeric: ", paste0(vars_dummies, collapse = ", ") ,". Check whether they should not be factors instead. As it stands, they are standardised, which is typically not recommended.")

  if (rename_std) {
    vars_num_sc <- paste0(vars_num, "_sd")
  } else {
    vars_num_sc <- vars_num
  }

  assign_scaled <- function(x, y) {
    assign(y, scale_blank(get(x, parent)), pos = here)
  }

  purrr::map2(vars_num, vars_num_sc, assign_scaled)

  other_vars <- setdiff(vars, vars_num)

  purrr::map(other_vars, ~ assign(.x, get(.x, parent), pos = here))

  if (!is.null(weights)) weights <- weights
  formula <- Reduce(paste, deparse(formula))

  if (rename_std) {
    repl <- paste0(vars_num, "_sd")
    names(repl) <- vars_num
    formula <- formula %>%
      stringr::str_replace_all(c(repl))
  }
  # formula <- as.formula(formula) #Rebuilds formula in current environment
  if (!is.null(weights)) {
    mod <- eval(parse(text = glue::glue("lm({formula}, weights = weights, ...)")))
  } else {
    mod <- eval(parse(text = glue::glue("lm({formula}, ...)")))
  }
  # mod <- lm(formula, weights = weights, ...)
  mod$call_fmt <- c(sys.call(), "Note: DV and continuous IVs were standardised")
  class(mod) <- c("rN_std", class(mod))
  mod
}

#' t-test() on multiply-imputed data (accepts survey weights)
#'
#' This runs t-test (based on lm(), therefore assuming equal variances) on multiple imputations,
#' with the ability to take into account survey weights.
#'
#' @param mi_list A list of dataframes, each consisting of a multiply imputed dataset
#' @param dv The dependent variable for the t.test (must be in mi_list)
#' @param groups The grouping variable (must have only two values, be in mi_list)
#' @param weights The variable containing survey weights, must be in mi_list
#'
#' @return A one-row tibble containing the result of the t-test
#'
#' @export

t_test_mi <- function(mi_list, dv, groups, weights = NULL) {
  dv <- rlang::enquo(dv)
  groups <- rlang::enquo(groups)
  weights <- rlang::enquo(weights)

  mi_list <- purrr::map(mi_list, dplyr::select, wt = !!weights, dv = !!dv, g = !!groups)

  out <- .run_t_test_mi(mi_list)

  out$group_var <- dplyr::as_label(groups)

  out
}

.run_t_test_mi <- function(mi_list) {
  tests <- purrr::map(mi_list, do.call, what = .wtd_t_test_lm) %>% mice::pool()

  res <- summary(tests)

  if (nrow(res) > 2) stop("Group should only have two levels - subset data or use pairwise_t_test_mi instead")

  groups <- mi_list[[1]]$g %>%
    unique() %>%
    as.character()

  out <- tibble::tibble(x = groups[1], y = groups[2], mean_diff = res[2, "estimate"], t_value = res[2, "statistic"], df = res[2, "df"], p_value = res[2, "p.value"])

  out
}

#' Pairwise t-tests() on multiply-imputed data (accepts survey weights)
#'
#' This runs pairwise t-tests (based on lm(), therefore assuming equal variances)
#' on multiple imputations, with the ability to take into account survey weights.
#'
#' @inheritParams t_test_mi
#' @param groups The grouping variable (each distinct value will be treated as a level)
#' @param p.adjust.method The method to adjust p-values for multiple comparison (see \code{\link[stats]{p.adjust}})
#' @return A tibble containing the results of the t-tests with one test per row
#'
#' @export

pairwise_t_test_mi <- function(mi_list, dv, groups, weights = NULL, p.adjust.method = p.adjust.methods) {
  if (!("quosure" %in% class(dv) | "name" %in% class(dv))) {
    dv <- rlang::enquo(dv)
    groups <- rlang::enquo(groups)
    weights <- rlang::enquo(weights)
  }

  pairs <- mi_list[[1]] %>%
    dplyr::select(!!groups) %>%
    dplyr::pull() %>%
    unique() %>%
    as.character() %>%
    utils::combn(2) %>%
    split(col(.))
  mi_list_sel <- purrr::map(mi_list, dplyr::select, wt = !!weights, dv = !!dv, g = !!groups)

  out <- purrr::map_df(pairs, function(x) {
    dat <- purrr::map(mi_list_sel, dplyr::filter, .data$g %in% x)
    .run_t_test_mi(dat)
  })

  out$p_value %<>% stats::p.adjust(p.adjust.method)

  out$group_var <- dplyr::as_label(groups)

  out
}

.wtd_t_test_lm <- function(dv, g, wt = NULL, ...) {
  lm(dv ~ g, weights = wt, ...)
}

#' Get letters to indicate results of multiple comparisons/post-hoc tests
#'
#' This takes the results of multiple comparisons and returns a set of letters
#' that can be used to indicate which differences are significant. The difference
#' between levels that are assigned the same letter are *not* statistically different.
#'
#' @param tests Either a tibble with the result of comparisons, including x and y
#' (the levels/groups that were compared) and p_value for the comparison or an object
#' of class pairwise.htest, for example returned from pairwise.t.test()
#' @param alpha_level The level of significance for the test
#' @param p.adjust.method One of p.adjust.methods, defaults to none as p-values will
#' typically have been adjusted when carrying out the pairwise comparisons/post-hoc tests
#'
#' @return A tibble with columns that indicate which letter has been assigned to each
#' group/level
#' @source Algorithm based on https://www.tandfonline.com/doi/abs/10.1198/1061860043515
#'
#' @examples
#' attach(airquality)
#' month <- factor(Month, labels = month.abb[5:9])
#' x <- pairwise.t.test(Ozone, Month)
#' library(rNuggets)
#' get_pairwise_letters(x)
#' detach()
#' @export


get_pairwise_letters <- function(tests,
                                 alpha_level = .05,
                                 p.adjust.method = "none") {
  if ("pairwise.htest" %in% class(tests)) {
    tests <- tests$p.value
    dat_levels <- c(colnames(tests), rownames(tests)) %>% unique()
    n1 <- nrow(tests)
    p <- cbind(rbind(NA, tests), NA)
    diag(p) <- 1
    p[upper.tri(p)] <- t(p)[upper.tri(p)]

    colnames(p) <- dat_levels
    rownames(p) <- dat_levels
    tests <- dat_levels %>%
      utils::combn(2) %>%
      split(col(.)) %>%
      purrr::map_df(function(a) tibble::tibble(x = a[1], y = a[2]))

    tests$p_value <- NA
    tests <- purrr::pmap_dfr(tests, function(...) {
      current <- tibble::tibble(...)
      current %>% dplyr::mutate(p_value = p[.data$x, .data$y])
    })
  }

  dat_levels <- c(tests$x, tests$y) %>%
    as.character() %>%
    unique()

  tests$p_value %<>% stats::p.adjust(p.adjust.method)

  tests %<>% dplyr::filter(.data$p_value < alpha_level)
  dat_letters <- tibble::tibble(dat_level = dat_levels)

  if (nrow(tests) == 0) { # If no comparisons are significant
    dat_letters[2] <- TRUE
  } else {
    dat_letters[2:(nrow(tests) + 2)] <- FALSE
    dat_letters[2] <- TRUE

    n <- 2

    for (i in 1:nrow(tests)) {
      for (j in 2:n) {
        if (dat_letters[dat_letters$dat_level == tests$x[i], j] &
          dat_letters[dat_letters$dat_level == tests$y[i], j]) {
          n <- n + 1
          #     print(paste0("Working on ", str_sub(x[1], start=-10), " and ", str_sub(x[2], start=-10), ". Copy column ", j, " to ", n,"."))
          dat_letters[n] <- dat_letters[j]
          dat_letters[dat_letters$dat_level == tests$x[i], j] <- FALSE
          dat_letters[dat_letters$dat_level == tests$y[i], n] <- FALSE
          # break
          # browser()
        }
      }
      # browser()
    }

    n <- 1
    absorb <- numeric()

    for (i in 2:(ncol(dat_letters) - 1)) {
      for (j in (i + 1):ncol(dat_letters)) {
        if (min(dat_letters[i] - dat_letters[j]) >= 0) {
          absorb[n] <- j
          n <- n + 1
        }
      }
    }

    if (length(absorb > 0)) dat_letters <- dat_letters[-absorb]


    n <- 1
    absorb <- numeric()

    for (i in (ncol(dat_letters):3)) {
      for (j in 2:(i - 1)) {
        if (min(dat_letters[i] - dat_letters[j]) >= 0) {
          absorb[n] <- j
          n <- n + 1
        }
      }
    }

    if (length(absorb > 0)) dat_letters <- dat_letters[-absorb]
  }
  for (i in 2:ncol(dat_letters)) {
    dat_letters[letters[i - 1]] <- NA_character_
    dat_letters[letters[i - 1]][dat_letters[[i]], ] <-
      letters[i - 1]
  }

  dat_letters %<>% dplyr::select(-dplyr::matches("^\\.")) %>%
    tidyr::unite("letters", -.data$dat_level, sep = "", remove = FALSE, na.rm = TRUE) %>%
    dplyr::rename(level = .data$dat_level)

  return(dat_letters)
}

#' Pairwise t-tests() returned in tidy dataframe
#'
#' This runs pairwise independent-samples t-tests (assuming unequal variance by default, but can be changed)
#' and returns the results and effect sizes in a tidy dataframe. Beware: It will automatically omit missing values.
#'
#' @param df A dataframe containing the outcome and grouping variable
#' @param outcome The outcome variable in dataframe
#' @param groups The grouping variable (each distinct value will be treated as a level)
#' @param p.adjust.method The method to adjust p-values for multiple comparison (see \code{\link[stats]{p.adjust}})
#' @param conf_level confidence level of the interval.
#' @param var_equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled
#' variance is used to estimate the variance otherwise the Welch (or Satterthwaite)
#' approximation to the degrees of freedom is used.
#' @return A tibble containing the results of the t-tests with one test per row, including a column (`apa`) formatted for reporting
#' @examples
#' \dontrun{
#' pairwise_t_tests(mtcars, wt, cyl)
#' }
#' @export

pairwise_t_tests <- function(df, outcome, groups, p.adjust.method = p.adjust.methods, conf_level = .95, var_equal = FALSE) {
  if(is.character(rlang::enexpr(outcome))) {
    warning("literal string input will eventually be deprecated across the package, please use raw variable names")
    outcome <- rlang::enexpr(outcome)
    groups <- rlang::enexpr(groups)
  }

  pairs <- df %>%
    dplyr::select({{groups}}) %>%
    dplyr::pull() %>%
    unique() %>%
    as.character() %>%
    utils::combn(2) %>%
    split(col(.))

  fmla <- as.formula(paste(dplyr::as_label(rlang::enexpr(outcome)), "~", dplyr::as_label(rlang::enexpr(groups))))

  out <- purrr::map_df(pairs, function(x) {
    dat <- dplyr::filter(df, {{groups}} %in% x)
    out <- stats::t.test(fmla, dat,
                  var.equal = var_equal, conf.level = conf_level,  na.action = "na.omit") %>% broom::tidy()
    desc <- dat %>% dplyr::arrange(dplyr::desc({{groups}} == x[1])) %>% dplyr::group_by({{groups}}) %>% dplyr::summarise(M = mean({{outcome}}, na.rm = TRUE), var = stats::var({{outcome}}, na.rm = TRUE), .groups = "drop")
    cohens_d <- (desc$M[1]-desc$M[2]) / sqrt((desc$var[1] + desc$var[2]) / 2)
    out <- cbind(tibble::tibble(var_1 = x[1], var_2 = x[2], cohens_d = cohens_d), out) %>%
      dplyr::select(.data$var_1, .data$var_2, mean_1 = .data$estimate1, mean_2 = .data$estimate2, mean_diff = .data$estimate, conf_low = .data$conf.low, conf_high = .data$conf.high, t_value = .data$statistic, df = .data$parameter, p_value = .data$p.value, .data$cohens_d,  test = .data$method)
    })

  out$p_value %<>% stats::p.adjust(p.adjust.method)
  out$p_value_adjust <- p.adjust.method[1]

  out$group_var <- dplyr::as_label(rlang::enexpr(groups))

  out$apa <- paste0("t(", round(out$df), ") = ", round_(out$t_value, 2), ", p ", fmt_p(out$p_value), ", d = ", round_(out$cohens_d, 2))

  out
}
