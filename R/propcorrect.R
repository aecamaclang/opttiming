#' Proportion of habitat correctly identified
#'
#' For a given rate of loss or growth \code{x}, determines the proportion of
#' habitat correctly identified at the optimal time under different learning
#' curves and threshold false positive rates (\eqn{\beta}).
#'
#' @aliases proplin
#' @aliases prophyp
#' @aliases propsig
#'
#' @param B The threshold false positive rate \eqn{\beta}.
#' @param t The amount of time to spend learning.
#' @param a The accuracy of identification at time t based on the learning curve used.
#' @param x A positive value for the rate of loss (if \code{q = 1}) or growth (if \code{q = -1}).
#' @param b,m Parameters defining the shape of the learning curves; if
#'   manually specified, overrides the default parameters.
#' @param q A value indicating whether optimal time should be calculated based
#'   on loss model (\code{q = 1}) or growth model (\code{q = -1}).
#' @name propcorrect
#'
#' @examples
#' proplin(B = 0.2, m = 0.2, a = 4, t = 3, x = 0.1)
#' prophyp(B = 0.5, b = 10, m = 5, a = 2, t = 5, x = 0.01)
#' propsig(B = 0.2, b = 9, m = 0.2, a = 6, t = 2, x = 0.5)
#'
#' @return The proportion correctly identified at the optimal time given the
#'   rate of loss (or growth) \code{x}.
NULL

#' @rdname propcorrect
#' @export
proplin <- function(B, m = 9/50, a, t, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(m == 9/50) message("Default linear function parameters used")
  
  if(x < -m*log(B)) {
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
#' @rdname propcorrect
#' @export
prophyp <- function(B, b = 9.25, m = 1, a, t, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(b == 9.25 || m == 1) message("Default hyperbolic curve parameters used")

  if(x < -(b/m)*log(B)) {
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
#' @rdname propcorrect
#' @export
propsig <- function(B, b = 10, m = 0.15, a, t, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(b == 10 || m == 0.15) message("Default sigmoid curve parameters used")

  if (x < (-((b-1)*m)/b)*log(B)) {
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
