#' Combine data frame with its 'class' and 'raw' predictions
.get_pred_matrix <- function(df, fit) {
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

#' Thin wrapper to run `mismm()` in tests
.run_mismm <- function(df,
                       features = 4:6,
                       ...) {
  df <- tibble::as_tibble(df)
  mismm(x = df[, features],
        y = df$bag_label,
        bags = df$bag_name,
        instances = df$instance_name,
        ...)
}

#' Thin wrapper to run `omisvm()` in tests
.run_omisvm <- function(df, .features = paste0("V", 1:5), .seed = 8, ...) {
  set.seed(.seed) # random selection of instances
  df <- tibble::as_tibble(df)
  omisvm(x = df[, .features],
         y = df$bag_label,
         bags = df$bag_name,
         ...)
}
