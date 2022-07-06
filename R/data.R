#' Sample ordinal MIL data using mvnorm
#'
#' A data set that demonstrates the ordinal multiple-instance learning
#' structure with feature columns randomly sampled from a multivariate normal
#' distribution.
#'
#' @format An MI data frame with 1000 rows 8 variables, and 5 bags. Instance
#'   labels can be accessed via `attr(ordmvnorm, "instance_label")`.
#' \describe{
#'     \item{bag_label}{outcome label at the bag level. This is the maximum of the `inst_label` for each bag}
#'     \item{bag_name}{indicator of each bag}
#'     \item{V1}{Variable with mean equal to `2 * inst_label`}
#'     \item{V2}{Variable with mean equal to `-1 * inst_label`}
#'     \item{V3}{Variable with mean equal to `1 * inst_label`}
#'     \item{V4}{Variable with mean 0, essentially noise}
#'     \item{V5}{Variable with mean 0, essentially noise}
#' }
"ordmvnorm"
