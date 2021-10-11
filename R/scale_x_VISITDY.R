# Shift positive values down
VISITDY_transform <- function(x) {
  x_i <-
    as.integer(
      # Set infinite to NA to prevent warnings
      ifelse(
        is.infinite(x),
        NA_integer_,
        x
      )
    )
  ifelse(
    is.infinite(x),
    as.integer(sign(x))*.Machine$integer.max,
    ifelse(
      x_i < 0,
      x_i,
      x_i - 1L
    )
  )
}

# Shift positive values up
VISITDY_inverse <- function(x) {
  ifelse(
    x < 0,
    x,
    x + 1L
  )
}

# Generate breaks daily then weekly, then with other choices
VISITDY_breaks <- function(n=5, ...) {
  n_default <- n
  function(x, n=n_default) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(numeric())
    }
    # Prefer daily then weekly then other options
    Q_choices <- c(1, 7, 2, 4, 3, 5)
    x_trans <- VISITDY_transform(x)
    rng <- range(x_trans)
    ret <- labeling::extended(dmin=rng[1], dmax=rng[2], m=n, Q=Q_choices, ...)
    rng_ret <- range(ret)
    if ((rng_ret[1] < 0) & (rng_ret[2] > 0)) {
      # Ensure that 1 is displayed
      ret <- sort(unique(c(-1L, 1L, ret)))
    }
    ret
  }
}

# Show every day with a minor break
VISITDY_minor_breaks <- function(b, limits, n) {
  b <- b[!is.na(b)]
  setdiff(seq(floor(limits[1]), ceiling(limits[2]), by=1), b)
}

VISITDY_trans <-
  scales::trans_new(
    name="VISITDY",
    transform=VISITDY_transform,
    inverse=VISITDY_inverse,
    breaks=VISITDY_breaks(5),
    minor_breaks=VISITDY_minor_breaks
  )

#' A scale for ggplots with SDTM day (DY) numbering
#' 
#' The scale will skip over 0, always show 1 (if it is in the range of the
#' data), provide breaks with preference toward week scales, and provide minor
#' breaks every day.
#' 
#' @param ... Passed to \code{ggplot2::scale_x_continuous()}
#' @examples
#' \dontrun{
#' ggplot(data.frame(x=-7:14, y=-7:14), aes(x=x, y=y, label=x)) +
#'   scale_x_VISITDY() +
#'   geom_text()
#' }
#' @export
scale_x_VISITDY <- function(...) {
  if ("trans" %in% ...names()) {
    stop("Cannot set 'trans' (just use scale_x_continuous())")
  }
  ggplot2::scale_x_continuous(..., trans=VISITDY_trans)
}
