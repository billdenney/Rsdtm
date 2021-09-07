#' Standardize the SDTM ID columns (STUDYID, USUBJID, and SUBJID)
#' 
#' @param data data.frame or similar to have standard IDs added
#' @param id a data.frame or list of data.frames to merge other identifiers into
#'   the data
#' @param studyid a character scalar indicating the study number
#' @param sep_usubjid a character scalar separating the STUDYID from the SUBJID
#' @param allow_missing_id Is it acceptable for some USUBJID and SUBJID to be
#'   missing?
#' @return A data.frame with columns for STUDYID, USUBJID, and SUBJID and other
#'   identifier columns removed.
#' @export
standardize_sdtm_id <- function(data, id, studyid, sep_usubjid="-", allow_missing_id=FALSE) {
  stopifnot("'sep_usubjid' must be a character"=is.character(sep_usubjid))
  stopifnot("'sep_usubjid' must not be NA"=!is.na(sep_usubjid))
  stopifnot("'sep_usubjid' must be a scalar"=length(sep_usubjid) == 1)
  # Track that no data were created or removed during this process (mainly to
  # confirm id merge accuracy)
  rowid_col <- paste0(max(names(data)), "X")
  data[[rowid_col]] <- seq_len(nrow(data))
  id_cols <- c("STUDYID", "USUBJID", "SUBJID")
  ret <- data
  if (!("STUDYID" %in% names(ret))) {
    stopifnot("'studyid' must be a character"=is.character(studyid))
    stopifnot("'studyid' must not be NA"=!is.na(studyid))
    stopifnot("'studyid' must be a scalar"=length(studyid) == 1)
    ret$STUDYID <- studyid
  }
  stopifnot("NA not allowed in STUDYID"=!any(is.na(ret$STUDYID)))
  if (!any(c("SUBJID", "USUBJID") %in% names(ret))) {
    if (missing(id)) {
      stop("'SUBJID' and 'USUBJID' columns are not in 'data' and no 'id' was provided")
    }
    if (is.data.frame(id)) {
      id <- list(id)
    }
    for (idx in seq_along(id)) {
      current_id <- id[[idx]]
      if (length(intersect(names(ret), setdiff(names(current_id), id_cols))) > 0) {
        ret <- dplyr::left_join(ret, current_id, by=intersect(names(ret), names(current_id)))
      }
      # Drop columns from id (e.g. RANDID) that are not STUDYID, SUBJID, or USUBJID
      ret <- ret[, setdiff(names(ret), setdiff(names(current_id), id_cols)), drop=FALSE]
    }
  }
  if (!allow_missing_id) {
    if ("SUBJID" %in% names(ret)) stopifnot("NA not allowed in SUBJID"=!any(is.na(ret$SUBJID)))
    if ("USUBJID" %in% names(ret)) stopifnot("NA not allowed in USUBJID"=!any(is.na(ret$USUBJID)))
  }
  if ("SUBJID" %in% names(ret) & !("USUBJID" %in% names(ret))) {
    # Put USUBJID in
    ret$USUBJID <- paste(ret$STUDYID, ret$SUBJID, sep=sep_usubjid)
  }
  if ("USUBJID" %in% names(ret) & !("SUBJID" %in% names(ret))) {
    ret$SUBJID <-
      substr(
        ret$USUBJID,
        start=nchar(ret$STUDYID) + nchar(sep_usubjid) + 1,
        stop=nchar(ret$USUBJID)
      )
  }
  if (!all(id_cols %in% names(ret))) {
    stop("Not all required columns are in the data.frame for return.  Report a bug.") # nocov
  }
  stopifnot("Likely merge error, different number of rows in output than input"=nrow(ret) == nrow(data))
  stopifnot("Likely subtle merge error, input rows are missing from output"=!any(duplicated(ret[[rowid_col]])))
  ret[, c(id_cols, setdiff(names(ret), c(rowid_col, id_cols)))]
}
