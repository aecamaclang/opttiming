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
#' @param b,m Parameters defining the shape of the learning curves; if
#'   manually specified, overrides the default parameters.
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
optlin <- function (B, m = 9/50, yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(m == 9/50) message("Default linear function parameters used")

  if (x < -m * log (B)) {
    (sqrt(-(log(B) * m)/(q*x)) - yint)/m
  } else (0)
}

#' @rdname opttime
#' @export
opthyp <- function (B, b = 9.25, m = 1, yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(b == 9.25 || m == 1) message("Default hyperbolic curve parameters used")

  if (x < -(b/m)*log(B)) {
    (sqrt((-log(B)*b*m)/(q*x))-yint*m)/(b+yint)
  } else (0)
}

#' @rdname opttime
#' @export
optsig <- function(B, b = 10, m = 0.15, yint = 1, q = 1, x) {
  if(B <= 0 || B >= 1) stop("B must be between 0 and 1")
  if(b == 10 || m == 0.15) message("Default sigmoid curve parameters used")

  if (x < (-((b-1)*m)/b)*log(B)) {
    -log((-b*(q*x)*yint)/(m*log(B)*(b-yint)))/m
  } else (0)
}
