#' Merge a supplementary dataset into a primary dataset
#'
#' @param primary the data.frame of the primary dataset
#' @param supplementary the data.frame of the supplementary dataset
#' @param remove_attributes If \code{TRUE}, remove all attributes from all
#'   columns (this will break many classes); if \code{FALSE}, remove no
#'   attributes from any columns; if a character, remove those attributes from
#'   the columns.
#' @return \code{primary} merged with \code{supplementary} where new column
#'   names come from the \code{QNAM} column in \code{supplementary}
#' @seealso \code{\link{supp_reformat}}
#' @export
#' @importFrom dplyr anti_join
merge_supp <- function(primary, supplementary, remove_attributes = c("label")) {
  if (length(unique(supplementary$RDOMAIN)) != 1) {
    stop("Only direct relationships with supplementary domains are currently supported.")
  }
  if (is.logical(remove_attributes) && remove_attributes) {
    primary <- strip_attributes(primary, specific = NULL)
    supplementary <- strip_attributes(supplementary, specific = NULL)
  } else if (is.character(remove_attributes)) {
    primary <- strip_attributes(primary, specific = remove_attributes)
    supplementary <- strip_attributes(supplementary, specific = remove_attributes)
  }
  ret <- primary
  supp_prep <- supp_reformat(supplementary)
  for (current_supp_idx in seq_along(supp_prep)) {
    current_supp <- supp_prep[[current_supp_idx]]
    current_idvar <- names(supp_prep)[current_supp_idx]
    current_join_vars <- c("STUDYID", "DOMAIN", "USUBJID", current_idvar)
    missed_rows <- dplyr::anti_join(current_supp, ret, by = current_join_vars)
    if (nrow(missed_rows)) {
      stop(
        nrow(missed_rows),
        " rows from the ",
        current_idvar,
        " IDVAR in the SUPP domain do not match rows in the primary ",
        primary$DOMAIN[1],
        " dataset."
      )
    }
    ret <- dplyr::left_join(ret, current_supp, by = current_join_vars)
  }
  ret
}

#' Reformat a --SUPP SDTM domain into a list of data.frames ready for merging
#' into the primary domain.
#'
#' @param x a --SUPP SDTM domain object
#' @param auto_convert should the data be automatically converted?
#' @return A list with length the same as \code{unique(x$IDVAR)} with
#'   data.frames ready for merging into the primary dataset.
#' @seealso \code{\link{merge_supp}}
#' @export
supp_reformat <- function(x, auto_convert=TRUE) {
  ret <- list()
  for (current_idvar in unique(x$IDVAR)) {
    ret <- append(
      ret,
      list(supp_reformat_single(x[x$IDVAR %in% current_idvar, ])))
  }
  names(ret) <- unique(x$IDVAR)
  ret
}

#' @importFrom dplyr rename_at
#' @importFrom tidyr spread
#' @importFrom readr type_convert
supp_reformat_single <- function(x, auto_convert=TRUE) {
  idvar <- unique(x$IDVAR)
  if (length(unique(x$RDOMAIN)) != 1) {
    stop("RDOMAIN column in x must have a single value.")
  } else if (length(idvar) != 1) {
    stop("IDVAR column in x must have a single value.")
  } else if (any(c("APID", "POOLID") %in% names(x))) {
    stop("APID and POOLID are not yet supported.")
  }
  # Columns to drop
  ret <- x[, setdiff(names(x), c("IDVAR", "QLABEL", "QORIG", "QEVAL")), drop=FALSE]
  ret <-
    rename_at(
      .tbl=ret,
      .vars=c("RDOMAIN", "IDVARVAL"),
      .funs=recode,
      RDOMAIN="DOMAIN",
      IDVARVAL=idvar
    )
  ret <-
    tidyr::spread(
      ret,
      key="QNAM",
      value="QVAL"
    )
  if (auto_convert) {
    ret <- type_convert(df=ret)
  }
  ret
}

#' Remove attributes from an object
#'
#' @param x The object to remove attributes from
#' @param specific If \code{NULL}, all attributes are removed.  If a character
#'   vector, only the named attributes are removed.
#' @param columns_only Do not strip attributes from the data.frame; only strip
#'   them from the columns of the data.frame.
#' @return \code{x} with fewer attributes.
#' @export
strip_attributes <- function(x, specific=NULL, ...) {
  UseMethod("strip_attributes")
}

strip_attributes.data.frame <- function(x, specific=NULL, columns_only=TRUE, ...) {
  if (columns_only) {
    for (nm in seq_along(x)) {
      x[[nm]] <- strip_attributes(x[[nm]], specific=specific, columns_only=columns_only, ...)
    }
    x
  } else {
    strip_attributes.default(x, specific=specific, ...)
  }
}

strip_attributes.default <- function(x, specific=NULL, ...) {
  if (is.null(specific)) {
    attributes(x) <- NULL
    x
  } else {
    for (current in specific) {
      attr(x, current) <- NULL
    }
    x
  }
}
