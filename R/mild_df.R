new_mild_df <- function(x = data.frame(), instance_label = NULL) {
  stopifnot(is.data.frame(x))
  stopifnot(is.vector(instance_label) || is.null(instance_label))

  structure(x,
    class = c("mild_df", "data.frame"),
    instance_label = instance_label
  )
}

validate_mild_df <- function(x) {
  instance_label <- attr(x, "instance_label")

  # Check column names
  if (any(colnames(x)[1:3] != c("bag_label", "bag_name", "instance_name"))) {
    rlang::abort(
      "The first three columns of `x` must be 'bag_label', 'bag_name', 'instance_name'"
    )
  }

  # Check whether `bag_label` is consistent with `bag_name`
  bags <- unique(x$bag_name)
  bag_labels <- function(x, bag) {
    labels <- x[which(x$bag_name == bag), "bag_label"]
    return(length(unique(labels)))
  }
  inconsistent_bag_labels <- sapply(bags, bag_labels, x = x) != 1

  if (any(inconsistent_bag_labels)) {
    rlang::abort(c(
      "`bag_label` must be consistent with `bag_name`.",
      i = paste0("Found inconsistency in ", sum(inconsistent_bag_labels), " bags."),
      x = paste0("'", bags[inconsistent_bag_labels][1], "' is the first inconsistent bag.")
    ))
  }

  # Check whether `instance_label` matches with `bag_label`, if provided
  if (!is.null(instance_label)) {

    check_inst_label <- function(x, bag, inst) {
      ind <- which(x$bag_name == bag)
      bag_label <- unique(x[ind, "bag_label"])
      inst_label <- inst[ind]
      return(max(inst_label) != bag_label)
    }
    inconsistent_inst_labels <- sapply(bags, check_inst_label,
                                       x = x, inst = instance_label)

    if (any(inconsistent_inst_labels)) {
      rlang::abort(c(
        "`bag_label` must be consistent with `instance_label`.",
        i = paste0("Found inconsistency in ", sum(inconsistent_inst_labels), " bags."),
        x = paste0("'", bags[inconsistent_inst_labels][1], "' is the first inconsistent bag.")
      ))
    }
  }

  # Check the there are two labels for bag_label
  n_lev <- length(unique(x$bag_label))
  if (n_lev != 2) {
    rlang::warn(c(
      "`bag_label` should have two levels.",
      x = paste("`bag_label` has", n_lev, "level(s).")
    ))
  }

  return(x)
}

#' Build a MILD data frame
#'
#' `mild_df()` constructs a data frame that corresponds to Multiple Instance
#' Learning with Distributional Instances (MILD) data.  A 'mild_df' object must
#' have three special columns:
#'   - 'bag_label', determines the label of each bag, typically from c(0, 1)
#'   - 'bag_name', character or factor that specifies the bag that each sample
#'   belongs to.
#'   - 'instance_name', character or factor that specifies the instance that
#'   each sample belongs to.
#'
#' We refer to the rows of a 'mild_df' as \emph{samples}, since they are
#' thought of as draws from the distribution that determines each instance.
#' Each instance is contained in a bag, with a corresponding label.  Instance
#' labels can be provided, but they will be pulled in as an attribute.
#'
#' @param bag_label A character, factor, or numeric vector
#' @param bag_name A character or factor vector
#' @param instance_name A character or factor vector
#' @param instance_label A character, factor, or numeric vector, or NULL
#'
#' @return A 'mild_df' object.
#'
#' @examples
#' mild_df('bag_label' = factor(c(1, 1, 0)),
#'         'bag_name' = c(rep('bag_1', 2), 'bag_2'),
#'         'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
#'         'X1' = c(-0.4, 0.5, 2),
#'         'instance_label' = c(0, 1, 0))
#'
#' @export
#' @author Yifei Liu, Sean Kent
mild_df <- function(bag_label = character(),
                    bag_name = character(),
                    instance_name = character(),
                    instance_label = NULL,
                    ...)
{
  x <- data.frame(
    bag_label = bag_label,
    bag_name = bag_name,
    instance_name = instance_name,
    ...
  )
  return(validate_mild_df(new_mild_df(x, instance_label = instance_label)))
}

#' Coerce to MILD data frame
#'
#' `as_mild_df()` turns an existing object, such as a data frame, into a MILD
#' data frame, a data frame with 'mild_df'. This is in contrast with
#' `mild_df()`, which builds a MILD data frame from individual columns.
#'
#' @inheritParams mild_df
#' @param bag_label A character (default 'bag_label') describing which column
#'   refers to the bag label.
#' @param bag_name A character (default 'bag_name') describing which column
#'   refers to the bag name
#' @param instance_name A character (default 'instance_name') describing which
#'   column refers to the instance name.
#' @param instance_label A character (default 'instance_label') describing which
#'   column refers to the instance labels. If NULL, no instance_labels will be
#'   used.
#' @param ... Unused
#'
#' @examples
#' x = data.frame('bag_LABEL' = factor(c(1, 1, 0)),
#'                'bag_name' = c(rep('bag_1', 2), 'bag_2'),
#'                'instance_name' = c('bag_1_inst_1', 'bag_1_inst_2', 'bag_2_inst_1'),
#'                'X1' = c(-0.4, 0.5, 2),
#'                'instance_label' = c(0, 1, 0))
#'
#' df <- as_mild_df(x)
#'
#' @export
#' @author Sean Kent
as_mild_df <- function(x, bag_label = "bag_label",
                       bag_name = "bag_name",
                       instance_name = "instance_name",
                       instance_label = "instance_label",
                       ...)
{
  UseMethod("as_mild_df")
}


#' @export
as_mild_df.default <- function(x,
                               bag_label = "bag_label",
                               bag_name = "bag_name",
                               instance_name = "instance_name",
                               instance_label = "instance_label",
                               ...)
{
  # TODO: allow `bag_label` to be passed as a vector
  if (!inherits(x, "data.frame")) {
    x <- as.data.frame(x)
  }
  cols <- colnames(x)

  # Check that `bag_label`, `bag_name`, `instance_name` exist in colnames(x)
  if (bag_label %ni% cols) {
    rlang::warn(c(
      paste("Column", bag_label, "not found in `x`."),
      i = paste("Using column,", cols[1], "as `bag_label`")
    ))
    bag_label <- cols[1]
  }
  if (bag_name %ni% cols) {
    rlang::warn(c(
      paste("Column", bag_name, "not found in `x`."),
      i = paste("Using column,", cols[2], "as `bag_name`")
    ))
    bag_name <- cols[2]
  }
  if (instance_name %ni% cols) {
    rlang::warn(c(
      paste("Column", instance_name, "not found in `x`."),
      i = paste("Using column,", cols[3], "as `instance_name`")
    ))
    instance_name <- cols[3]
  }

  # handle `instance_label` argument
  if (length(instance_label) == 1) {
    if (instance_label %in% cols) {
      inst_label_col <- which(cols == instance_label)
      instance_label <- x[[inst_label_col]]
      x <- x[, -inst_label_col, drop = FALSE]
      cols <- colnames(x)
    } else {
      rlang::inform(c(
        paste("Column", instance_label, "not found in `x`."),
        i = "Setting `instance_label` = NULL."
      ))
      instance_label <- NULL
    }
  }

  # Re-order and re-name the columns, if needed
  rest <- which(!(cols %in% c(bag_label, bag_name, instance_name)))
  bag_label <- which(cols == bag_label)
  bag_name <- which(cols == bag_name)
  instance_name <- which(cols == instance_name)

  x <- x[, c(bag_label, bag_name, instance_name, rest), drop = FALSE]
  colnames(x)[1:3] <- c("bag_label", "bag_name", "instance_name")

  return(validate_mild_df(new_mild_df(x, instance_label = instance_label)))
}
