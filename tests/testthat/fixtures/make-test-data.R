## test-cv_misvm --------------------------------------------------------------#

set.seed(8)
train_mild_df <- generate_mild_df(
  nbag = 20,
  nsample = 20,
  ncov = 3,
  nimp_pos = 1, nimp_neg = 1,
  positive_prob = 0.15,
  dist = rep("mvnormal", 3),
  mean = list(2, 0, 0),
  sd_of_mean = rep(0.1, 3)
)

train_df <- train_mild_df %>%
  summarize_samples() %>%
  dplyr::select(-instance_name)

set.seed(9)
test_mild_df <- generate_mild_df(
  nbag = 40,
  nsample = 20,
  ncov = 3,
  nimp_pos = 1, nimp_neg = 1,
  dist = rep("mvnormal", 3),
  mean = list(2, 0, 0),
  sd_of_mean = rep(0.1, 3)
)

test_df <- test_mild_df %>%
  summarize_samples() %>%
  dplyr::select(-instance_name)

saveRDS(train_mild_df, test_path("fixtures", "misvm-train_mild_df.rds"))
saveRDS(train_df, test_path("fixtures", "misvm-train_df.rds"))
saveRDS(test_mild_df, test_path("fixtures", "misvm-test_mild_df.rds"))
saveRDS(test_df, test_path("fixtures", "misvm-test_df.rds"))



## test-mismm -----------------------------------------------------------------#

set.seed(8)
train_mild_df <- generate_mild_df(
  nbag = 8,
  nsample = 4,
  ninst = 2,
  ncov = 3,
  nimp_pos = 1, nimp_neg = 1,
  dist = rep("mvnormal", 3),
  mean = list(15, 0, 0)
)

set.seed(9)
test_mild_df <- generate_mild_df(
  nbag = 20,
  nsample = 4,
  ninst = 2,
  ncov = 3,
  nimp_pos = 1, nimp_neg = 1,
  dist = rep("mvnormal", 3),
  mean = list(15, 0, 0)
)

saveRDS(train_mild_df, test_path("fixtures", "mimmm-train_mild_df.rds"))
saveRDS(test_mild_df, test_path("fixtures", "mimmm-test_mild_df.rds"))

