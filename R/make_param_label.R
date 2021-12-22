#' Generate a parameter label combining specimen type, parameter name, and units
#'
#' @param spec Specimen type (typically, "blood", "plasma", etc.)
#' @param param Parameter name (the thing that was measured)
#' @param unit Units of measure
#' @param expect_single Should a single parameter label be generated (or may it
#'   be a vector)?
#' @param allow_missing_spec,allow_missing_unit Are the \code{spec} or
#'   \code{unit} parameters allowed to be NA?
#' @return A character vector that is equivalent to \code{sprintf("%s %s (%s)",
#'   spec, param, unit)}, but accounting for the fact that spec and unit may be
#'   missing.
#' @export
make_param_label <- function(spec, param, unit, expect_single=TRUE,
                             allow_missing_spec=TRUE, allow_missing_unit=TRUE) {
  d <- data.frame(spec=spec, param=param, unit=unit)
  if (expect_single) {
    d <- unique(d)
    stopifnot("More than one parameter label created"=nrow(d) == 1)
  }
  stopifnot("spec may not be NA"=allow_missing_spec | !any(is.na(d$spec)))
  stopifnot("param may not be NA"=!any(is.na(d$param)))
  stopifnot("unit may not be NA"=allow_missing_unit | !any(is.na(d$unit)))
  # spec may be optional
  text_spec <- ifelse(is.na(d$spec), "", paste0(d$spec, " "))
  # unit may be optional
  text_unit <- ifelse(is.na(d$unit), "", sprintf(" (%s)", d$unit))
  paste0(text_spec, d$param, text_unit)
}
