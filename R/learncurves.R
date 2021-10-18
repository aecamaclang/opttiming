#' Learning curve functions
#'
#' For a given amount of time spent learning (\code{x}), determines
#' classification accuracy \code{a} under different learning curves.
#'
#' @aliases linear
#' @aliases hyperb
#' @aliases sigmoid
#'
#' @param x The amount of time spent learning.
#' @param yint The accuracy (\code{a}) of classification or identification at time
#'   \code{x = 0} (i.e., the y-intercept of the learning curve); default value
#'   \code{yint = 1}.
#' @param b,m Parameters defining the shape of the learning curves; if manually
#'   specified, overrides the default parameters or those set by specifying
#'   \code{hyp} or \code{sig}.
#' @name learncurves
#'
#' @note The learning curves used are as follows: \describe{
#'   \item{linear}{\code{a(x) = mlin * x + yint}} \item{hyperbolic}{\code{a(x) =
#'   ((bhyp * x)/(x + mhyp)) + yint}} \item{sigmoid}{\code{a(x) = b/(1 + (((b/yint) -
#'   1) * exp(-m * x)))}} }
#'
#' @examples
#' linear(x = 3)
#' hyperb(b = 9.25, m = 1, x = 1)
#' sigmoid(b = 10.75, m = 0.1, x = 5)
#'
#' @return The accuracy (\code{a}) of classification or identification at a
#'   given time \code{x}.
NULL

#' @rdname learncurves
#' @export
linear <- function(m=(9/50), yint = 1, x) {

  (m * x) + yint

}

#' @rdname learncurves
#' @export
hyperb <- function(b = NULL, m = NULL, yint = 1, x) {

  ((b * x)/(x + m)) + yint

}

#' @rdname learncurves
#' @export
sigmoid <- function(b = NULL, m = NULL, yint = 1, x) {

  b/(1 + (((b/yint) - 1) * exp(-m * x)))

}

