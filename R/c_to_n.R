#' Extract numeric values from character values
#'
#' @param x An object to convert from character to numeric using SDTM or ADaM
#'   rules.
#' @param ... Passed to other `c_to_n()` methods
#' @return For vectors, a numeric vector with `NA` at locations with non-numeric
#'   values.  For data.frames and similar, the same class of object with numeric
#'   versions of columns added.
#' @export
c_to_n <- function(x, ...)
  UseMethod("c_to_n")

#' @describeIn c_to_n The main workhorse function of c_to_n
#' 
#' @param pattern_numeric A regular expression to detect numeric values.
#' @param verbose Report detailed status of .
#' @details The main workhorse function is c_to_n.character, and it will
#'   recognize the following numeric types:
#' 
#' \itemize{
#'   \item{positive or negative (with optional sign) for all classes}
#'   \item{integers with an optional decimal point after the number}
#'   \item{floating point numbers without a number before the decimal point}
#'   \item{floating point numbers with a number before or after the decimal point}
#'   \item{scientific notation with any of the prior numeric types as the
#'     coefficient and an optional sign before the exponent}
#' }
#' 
#' Spaces at the beginning and end are removed with `trimws()`
#' 
#' @export
c_to_n.character <- function(x, ...,
                             pattern_numeric="^[+-]?(?:[0-9]+|[0-9]+\\.|\\.[0-9]+|[0-9]+\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$",
                             verbose=TRUE) {
  stopifnot(is.character(x))
  ret <- trimws(x)
  mask_numeric <- is.na(x) | grepl(pattern=pattern_numeric, x=ret)
  if (verbose) {
    if (all(mask_numeric)) {
      message("All values were converted to numeric.")
    } else {
      message(
        "The following character values were not converted to numeric: ",
        paste0('"', unique(x[!mask_numeric]), '"', collapse=", ")
      )
    }
  }
  ret[!mask_numeric] <- NA_character_
  as.numeric(ret)
}

#' @describeIn c_to_n For factors.
c_to_n.factor <- function(x, ...) {
  c_to_n.character(as.character(x), ...)
}

#' @describeIn c_to_n For numeric vectors.
c_to_n.numeric <- function(x, ...) {
  warning("`c_to_n` is generally not called on a numeric vector.  Please verify code.")
  x
}

#' @describeIn c_to_n For integer vectors.
c_to_n.integer <- function(x, ...) {
  warning("`c_to_n` is generally not called on an integer vector.  Please verify code.")
  x
}

#' @describeIn c_to_n For logical vectors (only handles all-NA case).
c_to_n.logical <- function(x, ...) {
  if (!all(is.na(x))) {
    warning("`c_to_n` does not set non-NA logical values to numeric.")
  }
  rep(NA_real_, length(x))
}

#' @describeIn c_to_n For data.frames and similar, finds columns matching the
#'   regular expression pattern `"^..(ST|OR).*C$"`.  data.frame method does not
#'   replace numeric columns that already exist.
c_to_n.data.frame <- function(x, ..., verbose=TRUE) {
  columns_named_c <- grep(pattern="^..(ST|OR).*C$", x=names(x), value=TRUE)
  columns_named_n <- grep(pattern="^..(ST|OR).*N$", x=names(x), value=TRUE)
  columns_prefix_c <- gsub(pattern="C$", replacement="", x=columns_named_c)
  columns_prefix_n <- gsub(pattern="N$", replacement="", x=columns_named_n)
  existing_n_columns <- intersect(columns_prefix_c, columns_prefix_n)
  prefixes_to_make_numeric <-
    if (length(existing_n_columns)) {
      message(
        "The following numeric columns already exist, not generating from the character equivalent: ",
        paste0(existing_n_columns, "N", collapse=", ")
      )
      setdiff(columns_prefix_c, columns_prefix_n)
    } else {
      columns_prefix_c
    }
  for (current_prefix in prefixes_to_make_numeric) {
    current_numeric <- paste0(current_prefix, "N")
    current_character <- paste0(current_prefix, "C")
    if (verbose) {
      message("Converting column ", current_character, " to ", current_numeric)
    }
    x[[current_numeric]] <- c_to_n(x[[current_character]])
  }
  x
}
