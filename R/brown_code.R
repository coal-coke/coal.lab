#' Form the digital code for brown coals in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{part 7})
#' form the digital code for brown coal using results of laboratory measurements.
#'
#' @param r
#'   reflectance of vitrinite, [\emph{\%}], measured in accordance with
#'   \strong{ISO 7404-5}.
#'   Type: [\code{double}].
#'
#' @param sok
#'   total volume of all \emph{fusinite} fractions, [\emph{\%}].
#'   The next \emph{fusinite} fractions may be considered:
#'   \describe{
#'     \item{\emph{inertinite}}{determined in accordance with \strong{ISO 7404-3}}
#'     \item{two thirds of \emph{semivitrinite}}{determined in accordance with \href{http://docs.cntd.ru/document/1200105478}{GOST R 55662}}
#'   }
#'   Type: [\code{numeric}].
#'
#' @param wmaxaf
#'   maximum \emph{moisture-holding capacity}, [\emph{\%}], measured in
#'   accordance with \strong{ISO 1018}. Type: [\code{double}].
#'
#' @param tscdaf
#'   \emph{yield of tar}, [\emph{\%}], measured in accordance with
#'   \strong{ISO 647} and recalculated on dry ash-free basis.
#'   Type: [\code{double}].
#'
#' @return
#'   Digital code according to \emph{Part 7} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#'  # Consider samples of brown coals with the next laboratory
#'  # measurement results:
#'  r0   <- c(.3)
#'  sok  <- c(6)
#'  wmaxaf <- c(53)
#'  tscdaf <- c(7)
#'
#'  x <- brown_code(r0, sok, wmaxaf, tscdaf)
#'  print(x)
#'
#'  # Unit test:
#'  #stopifnot(
#'  #  x %in% coal.state:::g25543db()$hard$S1KCFx$code
#'  #)
brown_code <- function(r, sok, wmaxaf, tscdaf){
  checkmate::assert_double(r, 0, 7, any.missing = FALSE, min.len = 1)
  n <- length(r)
  checkmate::assert_numeric(sok, 0, 100, any.missing = FALSE, len = n)
  checkmate::assert_numeric(wmaxaf, 0, 69.9444, any.missing = FALSE, len = n)
  checkmate::assert_double(tscdaf, 0, 100, len = n)

  code(fuel_class(r), fuel_cat(sok), brown_type(wmaxaf), brown_subtype(tscdaf))
}

#r0   <- c(.24)
#sok  <- c(6)
#wmaxaf <- c(53)
#tscdaf <- c(7)##

#x <- brown_code(r0, sok, wmaxaf, tscdaf)
#print(x)
#
#stopifnot(
#  x %in% coal.state:::g25543db()$brown$G1Ex$code
#)
