## code to prepare `ordmvnorm` dataset goes here
#' - 5 columns where 3 of them have means related to outcome and the other 2 are noise
#' - bags are aggregated randomly
#' - bag labels use standard assumption from `mildsvm::classify_bags()`

set.seed(8)

n <- 1000
bags <- rep(1:(n/5), each = 5)
y <- sample(1:5, size = n, prob = (1 / 1:5)^2, replace = TRUE)
y <- classify_bags(y, bags, condense = FALSE)

x <- matrix(NA, nrow = length(y), ncol = 5)
for (y_ in unique(y)) {
  to_fill <- which(y_ == y)
  x[to_fill, ] <- mvtnorm::rmvnorm(length(to_fill), mean = c(2*y_, -1*y_, 1*y_, 0, 0))
}
colnames(x) <- paste0("V", 1:ncol(x))

ordmvnorm <-
  dplyr::bind_cols(
    bag_label = classify_bags(y, bags, condense = FALSE),
    bag_name = bags,
    inst_label = y,
    as.data.frame(x)
  ) %>%
  tibble::as_tibble()

usethis::use_data(ordmvnorm, overwrite = TRUE)
