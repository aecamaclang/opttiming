#' Optimal time to spend learning
#'
#' For a given rate of loss or growth \code{x}, determines the optimal time to
#' spend learning under different learning curves and threshold false positive
#' rates (\eqn{\beta}).
#'
#' @aliases optlin
#' @aliases opthyp
#' @aliases optsig
#'
#' @param B The threshold false positive rate \eqn{\beta}.
#' @param yint The accuracy (\code{a}) of classification or identification at time
#'   \code{t = 0} (i.e., the y-intercept of the learning curve); default value
#'   \code{yint = 1}.
#' @param q A value indicating whether optimal time should be calculated based
#'   on a (positive) loss model (\code{q = 1}) or a (negative) growth model
#'   (\code{q = -1}).
#' @param x The rate of loss (if \code{q = 1}) or growth (if \code{q = -1}).
#' @param hyp The type of hyperbolic learning curve to be used with
#'   \code{prophyp()}.For \code{hyp = 1}, sets \code{bhyp = 9.25} and \code{mhyp
#'   = 1}; for \code{hyp = 2}, sets \code{bhyp = 10} and \code{mhyp = 5}.
#' @param sig The type of sigmoid learning curve to be used with
#'   \code{propsig()}. For \code{sig = 1}, sets \code{b = 10} and \code{m =
#'   0.15}; for \code{sig = 2}, sets \code{b = 10.75} and \code{m = 0.1}.
#' @param b,m Parameters defining the shape of the learning curves; if
#'   manually specified, overrides the default parameters or those set by specifying
#'   \code{hyp} or \code{sig}.
#' @name opttime
#'
#' @examples
#' optlin(B = 0.5, x = 0.001)
#' opthyp(B = 0.2, hyp = 1, x = 0.005)
#' optsig(B = 0.5, b = 10, m = 0.15, x = 0.002)
#'
#' @return The optimal amount of time to spend learning that maximizes the
#'   proportion correctly identified.
NULL

#' @rdname opttime
#' @export
optlin <- function (B, m = (9/50), yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")

  if (x < -m * log (B)) {
    # (sqrt((-log(B))/(q*x*mlin)))-(i/mlin)
    (sqrt(-(log(B) * m)/(q*x)) - yint)/m
  } else (0)
}

#' @rdname opttime
#' @export
opthyp <- function (B, hyp = NULL, b = NULL, m = NULL, yint = 1, q = 1, x) {
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
  if (x < -(b/m)*log(B)) {
    (sqrt((-log(B)*b*m)/(q*x))-yint*m)/(b+yint)
  } else (0)
}

#' @rdname opttime
#' @export
optsig <- function(B, sig = NULL, b = NULL, m = NULL, yint = 1, q = 1, x) {
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
    -log((-b*(q*x)*yint)/(m*log(B)*(b-yint)))/m
  } else (0)
}
