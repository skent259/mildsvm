new_mi_df <- function(x = data.frame(), instance_label = NULL) {
  stopifnot(is.data.frame(x))
  stopifnot(is.vector(instance_label) || is.null(instance_label))

  tibble::new_tibble(
    x,
    class = "mi_df",
    instance_label = instance_label
  )
}

validate_mi_df <- function(x) {
  instance_label <- df_instance_label(x)
  # Check column names
  if (any(colnames(x)[1:2] != c("bag_label", "bag_name"))) {
    rlang::abort(
      "The first two columns of `x` must be 'bag_label', 'bag_name'"
    )
  }

  # Check whether `bag_label` is consistent with `bag_name`
  bags <- unique(x$bag_name)
  bag_labels <- function(x, bag) {
    labels <- x[which(x$bag_name == bag), "bag_label", drop = TRUE]
    return(length(unique(labels)))
  }
  inconsistent_bag_labels <- sapply(bags, bag_labels, x = as.data.frame(x)) != 1

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
      bag_label <- unique(x[ind, "bag_label", drop = TRUE])
      inst_label <- inst[ind]
      return(max(inst_label) != bag_label)
    }
    inconsistent_inst_labels <- sapply(bags, check_inst_label,
                                       x = as.data.frame(x), inst = instance_label)

    if (any(inconsistent_inst_labels)) {
      rlang::abort(c(
        "`bag_label` must be consistent with `instance_label`.",
        i = paste0("Found inconsistency in ", sum(inconsistent_inst_labels), " bags."),
        x = paste0("'", bags[inconsistent_inst_labels][1], "' is the first inconsistent bag.")
      ))
    }
  }

  # Check the there are at least two labels for bag_label
  n_lev <- length(unique(x$bag_label))
  if (n_lev < 2) {
    rlang::warn(c(
      "`bag_label` should have at least two levels.",
      x = paste("`bag_label` has", n_lev, "level(s).")
    ))
  }

  return(x)
}

#' Build a multiple instance (MI) data frame
#'
#' `mi_df()` constructs a data frame that corresponds to Multiple Instance (MI)
#' data.  A `mi_df` object must have two special columns:
#' * `bag_label`, determines the label of each bag, typically from `c(0, 1)`
#' * `bag_name`, character or factor that specifies the bag that each sample
#'   belongs to.
#'
#' We refer to the rows of a `mi_df` as \emph{instances}. Each instance is
#' contained in a bag, with a corresponding label. Bags will typically have
#' several instances within them. Instance labels can be provided, but they will
#' be pulled in as an attribute.
#'
#' @param bag_label A `character`, `factor`, or `numeric` vector.
#' @param bag_name A `character` or `factor` vector.
#' @param instance_label A `character`, `factor`, or `numeric` vector, or
#'   `NULL`.
#' @param ... A set of name-value pairs. These construct the covariates for a
#'   `mi_df`.
#'
#' @return A 'mi_df' object. This data.frame-like has columns `bag_label`,
#'   `bag_name`, and those specified in `...`. It also inherits from the
#'   `'tbl_df'` and `'tbl'` classes.
#'
#' @seealso
#' * [as_mi_df()] to convert data.frames to `mi_df`s.
#'
#' @examples
#' mi_df('bag_label' = factor(c(1, 1, 0)),
#'       'bag_name' = c(rep('bag_1', 2), 'bag_2'),
#'       'X1' = c(-0.4, 0.5, 2),
#'       'instance_label' = c(0, 1, 0))
#'
#' @export
#' @author Sean Kent
mi_df <- function(bag_label = character(),
                  bag_name = character(),
                  ...,
                  instance_label = NULL) {
  x <- tibble::tibble(
    bag_label = bag_label,
    bag_name = bag_name,
    ...
  )
  return(validate_mi_df(new_mi_df(x, instance_label = instance_label)))
}

df_instance_label <- function(x) {
  if (inherits(x, "mi_df") || inherits(x, "mild_df")) {
    attr(x, "instance_label")
  } else {
    NULL
  }
}

#' Coerce to MI data frame
#'
#' `as_mi_df()` turns an existing object, such as a data frame, into a MI
#' data frame, a data frame with 'mi_df'. This is in contrast with
#' [mi_df()], which builds a MI data frame from individual columns.
#'
#' @param x A data-frame or similar to convert.
#' @param bag_label A character (default `'bag_label'`) describing which column
#'   refers to the bag label.
#' @param bag_name A character (default `'bag_name'`) describing which column
#'   refers to the bag name.
#' @param instance_label A character (default `'instance_label'`) describing which
#'   column refers to the instance labels. If NULL, no instance_labels will be
#'   used.
#' @param ... Arguments reserved for other methods.
#'
#' @return A 'mi_df' object. This data.frame-like has columns `bag_label`,
#'   `bag_name`, and potentially others. It also inherits from the
#'   `'tbl_df'` and `'tbl'` classes.
#'
#' @seealso [mi_df()] to build a `mi_df` object.
#' @examples
#' x = data.frame('bag_LABEL' = factor(c(1, 1, 0)),
#'                'bag_name' = c(rep('bag_1', 2), 'bag_2'),
#'                'X1' = c(-0.4, 0.5, 2),
#'                'instance_label' = c(0, 1, 0))
#'
#' df <- as_mi_df(x)
#'
#' @export
#' @author Sean Kent
as_mi_df <- function(x,
                     bag_label = "bag_label",
                     bag_name = "bag_name",
                     instance_label = "instance_label",
                     ...) {
  UseMethod("as_mi_df")
}

#' @export
as_mi_df.default <- function(x,
                             bag_label = "bag_label",
                             bag_name = "bag_name",
                             instance_label = "instance_label",
                             ...) {
  if (!inherits(x, "data.frame")) {
    x <- as.data.frame(x)
  }
  cols <- colnames(x)

  bag_label <- .check_val_in_cols(bag_label, cols, default = cols[1])
  bag_name <- .check_val_in_cols(bag_name, cols, default = cols[2])

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
  rest <- which(!(cols %in% c(bag_label, bag_name)))
  bag_label <- which(cols == bag_label)
  bag_name <- which(cols == bag_name)

  x <- x[, c(bag_label, bag_name, rest), drop = FALSE]
  colnames(x)[1:2] <- c("bag_label", "bag_name")

  return(validate_mi_df(new_mi_df(x, instance_label = instance_label)))
}

#' Printing multiple instance data frames
#'
#' @description
#' Specialized print methods for the `mi_df`, `mild_df` classes. These return
#' helpful information such as the number of rows, columns, bags, and instances
#' (for `mild_df` objects).
#'
#' These methods print the data frame based on the underlying subclass. This
#' allows for additional arguments that can be passed to `print.tbl()` when the
#' subclass is a tibble (`tbl_df`, `tbl`), documented below.
#'
#' @details
#' The following extra arguments are available when `x` has subclass `tbl`:
#' - `n`: Number of rows to show. If `NULL`, the default, will print all rows
#'   if less than the `print_max` [option][pillar::pillar_options]. Otherwise,
#'   will print as many rows as specified by the `print_min`
#'   [option][pillar::pillar_options].
#' - `width`: Width of text output to generate. This defaults to `NULL`, which
#'   means use the `width` [option][pillar::pillar_options].
#' - `max_extra_cols`: Number of extra columns to print abbreviated
#'   information for, if the width is too small for the entire tibble. If
#'   `NULL`, the `max_extra_cols` [option][pillar::pillar_options] is used. The
#'   previously defined `n_extra` argument is soft-deprecated.
#' - `max_footer_lines`: Maximum number of footer lines. If `NULL`, the
#'   `max_footer_lines` [option][pillar::pillar_options] is used.
#'
#' @param x Object to format or print.
#' @param ... Passed to other methods.  See [print.tbl()] or details for more
#'   information.
#'
#' @return The object passed in `x`, invisibly. Primarily called to print the
#'   object to the console.
#'
#' @examples
#' data("ordmvnorm")
#' print(as_mi_df(ordmvnorm, instance_label = "inst_label"))
#'
#' print(as_mi_df(ordmvnorm, instance_label = "inst_label"), n = 2)
#'
#' @name formatting
#' @aliases print.mi_df print.mild_df
NULL

#' @rdname formatting
#' @export
print.mi_df <- function(x, ...) {
  if (!inherits(x, "tbl")) {
    str <- .make_mi_df_header(x)
    cat(str[1])
    if (!is.null(attr(x, "instance_label"))) {
      cat(str[2])
    }
  }
  NextMethod()
}

#' @export
#' @importFrom pillar tbl_sum
tbl_sum.mi_df <- function(x, ...) {
  .make_mi_df_header(x)
}

#' @export
`[.mi_df` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod("[")
  if (!missing(j)) {
    warn <- length(j) > 1
  } else {
    warn <- FALSE
  }

  if (nargs() > 2) {
    inst_label <- df_instance_label(x)
    if (!is.null(inst_label)) {
      attr(out, "instance_label") <- inst_label[i]
    }
  }
  .drop_class_if_metadata_removed(out, "mi_df", warn)
}

## Utility functions below ----------------------------------------------------#

#' Check for `val` in `cols`
#' @param val A character to check
#' @param cols A character vector of column names
#' @param default An integer which specifies the element in `cols` to default to
#' @noRd
.check_val_in_cols <- function(val, cols, default) {
  if (val %ni% cols) {
    rlang::warn(c(
      paste("Column", val, "not found in `x`."),
      i = paste0("Using column, ", default, " as `", val, "`")
    ))
    val <- default
  }
  val
}

#' Make header for printing
#'
#' Should look like:
#' ```
#' An MI data frame: 3 x 3 with 2 bags
#' and instance labels: 0, 1, 0
#' ```
#' @param x An `mi_df` object
#' @noRd
.make_mi_df_header <- function(x) {
  n_bag <- length(unique(x$bag_name))
  str1 <- paste("An MI data frame:", pillar::dim_desc(x),
                "with", n_bag, "bags", "\n")

  if (!is.null(attr(x, "instance_label"))) {
    inst <- attr(x, "instance_label")
    if (length(inst) > 5) {
      inst_str <- paste0(inst[1:5], collapse = ", ")
      inst_str <- paste0(inst_str, ", ...")
    } else if (length(inst) == 5) {
      inst_str <- paste0(inst[1:5], collapse = ", ")
    } else {
      inst_str <- paste0(inst, collapse = ", ")
    }
    str2 <- paste("and instance labels:", inst_str, "\n")
  } else {
    str2 <- NULL
  }
  c(str1, str2)
}

#' Drop `mi_df` class if metadata columns were removed from the data frame
#' @noRd
.drop_class_if_metadata_removed <- function(x, class = "mi_df", warn = TRUE) {
  class <- match.arg(class, c("mi_df", "mild_df"))
  if (!all(.reserved_df_variables(class) %in% names(x))) {
    if (warn) {
      msg <- paste0("Dropping '", class, "' class as required column was removed.")
      rlang::warn(msg)
    }
    class(x) <- setdiff(class(x), class)
  }
  x
}

#' @noRd
.reserved_df_variables <- function(class = "mi_df") {
  class <- match.arg(class, c("mi_df", "mild_df"))
  switch(
    class,
    "mi_df" = c("bag_label", "bag_name"),
    "mild_df" = c("bag_label", "bag_name", "instance_name")
  )
}
