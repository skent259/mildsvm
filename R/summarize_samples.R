#' Summarize data across functions
#'
#' Summarize a numeric data frame based on specified grouping columns and a list
#' of functions.  This is useful in summarizing a `mild_df` object from the
#' sample level to the instance level.
#'
#' @param data A data.frame, 'mild_df' object, or similar of data to summarize.
#' @param group_cols A character vector of column(s) that describe groups to
#'   summarize across.
#' @param .fns A list of functions (default `list(mean = mean)`).
#' @param cor A logical (default `FALSE`) for whether to include correlations
#'   between all features in the summarization.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A tibble with summarized data.  There will be one row for each set
#'   of distinct groups specified by `group_cols`. There will be one column for
#'   each of the `group_cols`, plus `length(.fns)` columns for each of the
#'   features in `data`, plus correlation columns if specified.
#'
#' @examples
#' fns <- list(mean = mean, sd = sd)
#' summarize_samples(mtcars, group_cols = c("cyl", "gear"), .fns = fns)
#' summarize_samples(mtcars, group_cols = c("cyl", "gear"), .fns = fns, cor = TRUE)
#'
#' @author Sean Kent
#' @name summarize_samples
NULL

#' @export
summarize_samples <- function(data, group_cols, .fns, cor = FALSE, ...) {
  UseMethod("summarize_samples")
}

#' @describeIn summarize_samples Method for data.frame-like objects.
#' @export
summarize_samples.default <- function(data,
                                      group_cols,
                                      .fns = list(mean = mean),
                                      cor = FALSE,
                                      ...) {
  df <- data %>%
    dplyr::group_by(dplyr::all_of(dplyr::across(group_cols))) %>%
    dplyr::summarize_all(.fns) %>%
    dplyr::ungroup()

  if (cor) {
    df <- dplyr::left_join(df, .compute_cor(data, group_cols), by = group_cols)
  }
  return(df)
}

#' @describeIn summarize_samples Method for `mild_df` objects.
#' @export
summarize_samples.mild_df <- function(data, ...) {
  group_cols <- c("bag_label", "bag_name", "instance_name")
  return(summarize_samples.default(data, group_cols, ...))
}

# Additional internal functions below ------------------------------------------

#' Compute correlations between all features of a data.frame
#' @noRd
.compute_cor <- function(data, group_cols) {
  data %>%
    dplyr::group_by(dplyr::all_of(dplyr::across(group_cols))) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      cov = purrr::map(data, stats::cov),
      cov_var = purrr::map(.data$cov, ~.x[upper.tri(.x)])
    ) %>%
    tidyr::unnest_wider(.data$cov_var, names_sep = "_") %>%
    dplyr::select(-.data$data, -.data$cov) %>%
    dplyr::ungroup()
}
