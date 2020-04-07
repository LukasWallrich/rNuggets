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
    print(paste("Cohen's d:", round(cohens_d, 3)))
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
    df <- eval(parse(text = paste0("update(df, filt = factor(df$variables$",
                                   iv, ", levels = c('", paste0(pair,
                                   collapse = "', '"), "')))")))
    t.test_result <- NULL
    if (ttest) {
        print(paste0("T-Test for ", paste0(pair, collapse = " & "), ":"))
        t.test_result <- eval(parse(text = paste0("survey::svyttest(", dv, " ~ ", iv,
                                                  ", subset(df, !is.na(filt)))")))
        print(t.test_result)
    }
    # Calculate Cohen's d
    means <- survey::svyby(~get(dv), ~filt, df, survey::svymean)[1:2]
    names(means) <- c(dv, "mean")
    vars <- survey::svyby(~get(dv), ~filt, df, survey::svyvar)[1:2]
    names(vars) <- c(dv, "var")
    cohens_d <- (means[1, 2] - means[2, 2]) / sqrt((vars[1, 2] + vars[2, 2]) / 2)
    print(paste0("Cohen's d for pair ", paste0(pair, collapse = " & "), " is:",
                 round(cohens_d, 3)))
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
    if (is.null(cats))
        cats <- eval(parse(text = paste0("levelsdf$variables$", iv)))

        print(paste("Beware - NO correction for multiple comparisons is employed.\n",
                factorial(length(cats)) / (factorial(length(cats) - 2) * 2),
                "pairwise tests will be performed."))
    df2 <- purrr::cross_df(data.frame(cats, cats, stringsAsFactors = F), .filter =
                        function(x, y) as.character(x) <= as.character(y))
    x <- purrr::map(purrr::pmap(df2, c), function(x) svy_cohen_d_pair(pair = x, ...))

    names(x) <- tidyr::unite(df2, .data$new_col)[[1]]
    x
}

#' lm() with standardised continuous variables - no data argument
#'
#' This runs lm() after standardising all continuous variables, while leaving
#' factors intact. It does not take a data argument, so it is intended to be used with
#' with() - if a data argument is needed, use `run_lm()` instead.
#'
#' @inheritParams stats::lm
#' @param rename_std Logical. Should standardised variables be indicated by _sd
#' suffix
#' @inheritDotParams stats::lm -data
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. If you want to run a model with all variables standardised,
#' one option is `QuantPsyc::lm.beta()`
#' @export

lm_std <- function(formula, weights = NULL, rename_std = FALSE, ...) {
    parent <- parent.frame()
    here <- environment()
    vars <- all.vars(formula)

    vars_num <- vars[purrr::map_lgl(vars, ~is.numeric(get(.x, parent)))]

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

    purrr::map(other_vars, ~assign(.x, get(.x, parent), pos = here))

    if (!is.null(weights)) weights <- weights
    formula <- Reduce(paste, deparse(formula))

    if (rename_std) {
    repl <- paste0(vars_num, "_sc")
    names(repl) <- vars_num
   formula <- formula %>%
        stringr::str_replace_all(c(repl))
    }
    formula <- as.formula(formula) #Rebuilds formula in current environment

    mod <- lm(formula, weights = weights, ...)
    mod$call_fmt <- c(sys.call(), "Note: DV and continuous IVs were standardised")
    class(mod) <- c(class(mod), "rN_std")
    mod

}

