#' Import an SDTM or ADaM file or directory of files
#'
#' @param path The path to the directory or file.
#' @param extension_choice What file extension(s) should be searched for within
#'   the directory?  (If more than one file with the same extension exists, the
#'   first extension in \code{extension_choice} will be used.
#' @param ignore_case Passed to \code{list.files} when loading a directory.
#' @param return_type When loading a single file, what type of output should be
#'   provided?
#' @param ... Arguments passed to \code{rio::import}
#' @return If a directory or a vector of files is given in \code{path}, a list
#'   of data.frames; the names of the list will be the uppercase basename of the
#'   file without the extension.  If a single file is given in \code{path}, a
#'   data.frame.
#' @details It is an error to import the same filename base (filename without
#'   path or extension) more than once.  That can happen by loading more than
#'   one file with the same name from different directories or multiple versions
#'   of the same file.
#' @export
import_sdtm <- function(path,
                        extension_choice = c(".sas7bdat", ".xpt"),
                        ignore_case = TRUE,
                        ...) {
  stopifnot(
    is.character(path),
    all(!is.na(path))
  )
  mask_dir <- dir.exists(path)
  mask_file <- file.exists(path) & !mask_dir
  mask_none <- !(mask_dir | mask_file)
  if (any(mask_none)) {
    stop(
      "The following were not found as directories or files: ",
      paste0('"', path[mask_none], '"', collapse = ", ")
    )
  }
  ret <- list()
  for (dir_idx in which(mask_dir)) {
    tmp_ret <- import_sdtm_dir(
      path = path[dir_idx],
      extension_choice = extension_choice,
      ignore_case = ignore_case,
      ...
    )
    ret <- append_no_duplicate_names(ret, tmp_ret)
  }
  for (file_idx in which(mask_file)) {
    tmp_ret <- import_sdtm_file(
      path = path[file_idx],
      return_type = "list",
      ...
    )
    ret <- append_no_duplicate_names(ret, tmp_ret)
  }
  ret
}

#' @describeIn import_sdtm Load a directory of SDTM files.
#' @export
import_sdtm_dir <- function(path,
                            extension_choice = c(".sas7bdat", ".xpt"),
                            ignore_case = TRUE,
                            ...) {
  stopifnot(length(path) == 1, all(!is.na(path)))
  ret <- list()
  for (current_ext in extension_choice) {
    current_ext_pattern <-
      gsub(".", "\\.", current_ext, fixed = TRUE)
    current_ext_pattern <- paste0(current_ext_pattern, "$")
    current_files <-
      list.files(
        path = path,
        pattern = current_ext_pattern,
        full.names = TRUE,
        include.dirs = FALSE,
        ignore.case = ignore_case
      )
    for (current_file in current_files) {
      tmp_ret <- import_sdtm_file(
        path = current_file,
        return_type = "list",
        ...
      )
      ret <- append_no_duplicate_names(ret, tmp_ret)
    }
  }
  ret
}

#' @describeIn import_sdtm Load a single SDTM file.
#' @importFrom rio import
#' @export
import_sdtm_file <- function(path,
                             return_type = c("data.frame", "list"),
                             ...) {
  return_type <- match.arg(return_type)
  ret <- rio::import(file = path, ...)
  if (return_type == "list") {
    ret <- stats::setNames(
      object = list(ret),
      nm = detect_domain(ret, path)
    )
  }
  ret
}

#' Add elements to a vector, only if names are not duplicated in \code{x} and
#' \code{values}
#' @param x,values,after See \code{base::append}
#' @param ignore_blank Ignore blank names? (Blank names \code{""} occur when an
#'   unnamed vector is appeneded to a named vector.)
#' @return See \code{base::append}
append_no_duplicate_names <- function(x, values, after = length(x), ignore_blank = TRUE) {
  if (!(is.null(names(x)) |
    is.null(names(values)))) {
    names_x <- names(x)
    names_values <- names(values)
    if (ignore_blank) {
      names_x <- setdiff(names_x, "")
      names_values <- setdiff(names_values, "")
    }
    name_overlap <- intersect(names_x, names_values)
    if (length(name_overlap)) {
      stop(
        "The following names are duplicated: ",
        paste(name_overlap, collapse = ", ")
      )
    }
  }
  append(x = x, values = values, after = after)
}
