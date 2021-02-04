#' Form the digital code for hard coals in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{part 7})
#' form the digital code for \emph{hard coal} using results of laboratory
#' measurements.
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
#' @param vdaf
#'   \emph{volatile matter yield}, [\emph{\%}], measured in accordance with
#'   \strong{ISO 562} or \strong{ISO 5071-1} and recalculated
#'   on dry ash-free basis. Type: [\code{double}].
#'
#' @param y
#'   plastometric index \emph{y}, [\emph{mm}], measured in accordance with
#'   \href{http://docs.cntd.ru/document/1200121074}{GOST 1186}.
#'   Type: [\code{double}].
#'
#' @param sl
#'   \emph{crucible swelling number}, [], measured in accordance with
#'   \strong{ISO 501}. Type: [\code{double}].
#'
#' @return
#'   Digital code according to \emph{Part 7} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' # Consider five samples of hard coals with the next laboratory
#' # measurement results:
#' r0   <- c(.43, .64, .87, 1.1, 4.6)
#' sok  <- c(5.3, 2, 3, 24, 21)
#' vdaf <- c(41.8, 38.6, 37.3, 26.3, 8.1)
#' y <- c(0, 13, 26, 12, 6)
#' # Since crucible swelling number is rarely measured in laboratories
#' # they do not specify its value in this example
#'
#' x <- hard_code(r0, sok, vdaf, y)
#' print(x)
#' # [1] "0404000" "0603813" "0803626" "1122612" "4620806
#'
#' # Unit test:
#' stopifnot(
#'   x %in% with(coal.state:::g25543db()$hard, {
#'     c(
#'       SDB04x$code, G2Gx$code, G2X81x$code, S1KOBx$code, S2OCBx$code
#'     )
#'   })
#' )
hard_code <- function(r, sok, vdaf, y, sl = rep_len(0, length(r))){
  checkmate::assert_double(r, 0, 7, any.missing = FALSE, min.len = 1)
  n <- length(r)
  checkmate::assert_numeric(sok, 0, 100, any.missing = FALSE, len = n)
  checkmate::assert_double(vdaf, 8, 100, len = n)
  checkmate::assert_numeric(y, 0, 99, len = n)
  checkmate::assert_numeric(sl, 0, 9, len = n)

  code(fuel_class(r), fuel_cat(sok), hard_type(vdaf), hard_subtype(y, sl))
}

