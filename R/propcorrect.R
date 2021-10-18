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
#' @param yint The accuracy (\code{a}) of classification or identification at time
#'   \code{t = 0} (i.e., the y-intercept of the learning curve); default value
#'   \code{yint = 1}.
#' @param q A value indicating whether optimal time should be calculated based
#'   on loss model (\code{q = 1}) or growth model (\code{q = -1}).
#' @param x TThe rate of loss (if \code{q = 1}) or growth (if \code{q = -1}).
#' @param hyp The type of hyperbolic learning curve to be used with
#'   \code{prophyp()}.For \code{hyp = 1}, sets \code{bhyp = 9.25} and \code{mhyp
#'   = 1}; for \code{hyp = 2}, sets \code{bhyp = 10} and \code{mhyp = 5}.
#' @param sig The type of sigmoid learning curve to be used with
#'   \code{propsig()}. For \code{sig = 1}, sets \code{b = 10} and \code{m =
#'   0.15}; for \code{sig = 2}, sets \code{b = 10.75} and \code{m = 0.1}.
#' @param b,m Parameters defining the shape of the learning curves; if
#'   manually specified, overrides the default parameters of those set by specifying
#'   \code{hyp} or \code{sig}.
#' @name propcorrect
#'
#' @examples
#' proplin(B = 0.2, x = 5)
#' prophyp(B = 0.5, hyp = 2, x = 1)
#' propsig(B = 0.2, b = 9, m = 0.2, x = 0.5)
#'
#' @return The proportion correctly identified at the optimal time given the
#'   rate of loss (or growth) \code{x}.
NULL

#' @rdname propcorrect
#' @export
proplin <- function(B, m = (9/50), yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(x < -m*log(B)) {
    t <- optlin(B = B, m = m, yint = yint, q = 1, x = x)
    a <- linear(m = m, yint = yint, x = t)
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
#' @rdname propcorrect
#' @export
prophyp <- function(B, hyp = NULL, b = NULL, m = NULL, yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")

  if(is.null(b) || is.null(m)) {
    if(is.null(hyp) || (hyp != 1 && hyp != 2)) stop("hyperbolic curve parameters not specified") else {
      if(hyp == 1) {
        b <- 9.25
        m <- 1
      } else {
        if(hyp == 2) {
          b <- 10
          m <- 5
        }
      }
    }
  }

  if(x < -(b/m)*log(B)) {
    t <- opthyp(B = B, hyp = hyp, b = b, m = m, yint = yint, q = 1, x = x)
    a <- hyperb(b = b, m = m, yint = yint, x = t)
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
#' @rdname propcorrect
#' @export
propsig <- function(B, sig = NULL, b = NULL, m = NULL, yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")

  if(is.null(b) || is.null(m)) {
    if(is.null(sig) || (sig != 1 && sig != 2)) stop("sigmoidal curve parameters not specified") else {
      if(sig == 1) {
        b <- 10
        m <- 0.15
      } else {
        if(sig == 2) {
          b <- 10.75
          m <- 0.1
        }
      }
    }
  }
  if (x < (-((b-1)*m)/b)*log(B)) {
    t <- optsig(B = B, b = b, m = m, yint = yint, q = q, x = x)
    a <- sigmoid(b = b, m = m, yint = yint, x = t)
    exp(-(q*x) * t) * B^(1/a)
  } else (B)
}
