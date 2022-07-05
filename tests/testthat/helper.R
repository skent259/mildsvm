#' Combine data frame with its 'class' and 'raw' predictions
.get_pred_matrix <- function(df, fit) {
  pred <-
    df %>%
    dplyr::bind_cols(predict(fit, new_data = df)) %>%
    dplyr::bind_cols(predict(fit, new_data = df, type = "raw"))
}

#' Summarize predictions
.summarize_preds <- function(pred, by = bag_name) {
  pred %>%
    dplyr::group_by({{ by }}) %>%
    dplyr::distinct(bag_label, .pred, .pred_class)
}

#' Thin wrapper to run `cv_misvm()` in tests
.run_cv_misvm <- function(df = df1,
                         features = 3:5,
                         n_fold = 3,
                         cost_seq = 2^c(-2, 4),
                         ...) {
  cv_misvm(x = df[, features],
           y = df$bag_label,
           bags = df$bag_name,
           n_fold = n_fold,
           cost_seq = cost_seq,
           ...)
}

