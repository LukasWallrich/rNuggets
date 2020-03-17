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
#'   Cohen's d is calculated.
#' @return Invisibly returns a list including the t.test() output and
#'   Cohen's d

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
#' @return Invisibly returns a names lists of lists including the t.test()
#'   output and Cohen's D for each pair

## ToDos:
### allow this to run without printing all results
### improve return with broom

svy_pairwise.t.test <- function(cats, ...) {
    if (is.null(cats))
        cats <- eval(parse(text = paste0("levelsdf$variables$", iv)))

        print(paste("Beware - NO correction for multiple comparisons is employed.\n",
                factorial(length(cats)) / (factorial(length(cats) - 2) * 2),
                "pairwise tests will be performed."))
    df2 <- purrr::cross_df(data.frame(cats, cats, stringsAsFactors = F), .filter =
                        function(x, y) as.character(x) <= as.character(y))
    x <- purrr::map(purrr::pmap(df2, c), function(x) svy_cohen_d_pair(pair = x, ...))

    names(x) <- tidyr::unite(df2, .data$new_col)[[1]]
}
