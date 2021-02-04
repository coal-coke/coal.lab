#' Form the digital code for anthracite in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{part 7})
#' form the digital code for \emph{anthracite} using results of laboratory
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
#' @param vnudaf
#'   \emph{volatile matter volume yield}, [\emph{cm^3/g}], measured in
#'   accordance with \href{http://docs.cntd.ru/document/1200024047}{GOST 7303}.
#'   Type: [\code{double}].
#'
#' @param ar
#'   anisotropy of vitrinite reflectance, [\emph{\%}], calculated in accordance
#'   with formula 4 in \href{http://docs.cntd.ru/document/1200105476}{GOST R 55659}.
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
#' # Consider two samples of anthracite with the next laboratory
#' # measurement results:
#' r0   <- c(3.51, 3.64)
#' sok  <- c(12, 2)
#' vnudaf <- c(270.3, 174.6)
#' ar <- c(0, 46.8)
#'
#' x <- anthracite_code(r0, sok, vnudaf, ar)
#' print(x)
#' # [1] "3512020" "3601540"
#'
#' # Unit test:
#' stopifnot(
#'   x %in% with(coal.state:::g25543db()$anthracite, {
#'     c(
#'       S1ABx$code, S2ABx$code
#'     )
#'   })
#' )
anthracite_code <- function(r, sok, vnudaf, ar){
  checkmate::assert_double(r, 0, 7, any.missing = FALSE, min.len = 1)
  n <- length(r)
  checkmate::assert_numeric(sok, 0, 100, any.missing = FALSE, len = n)
  checkmate::assert_double(vnudaf, 0, 500, len = n)
  checkmate::assert_numeric(ar, 0, 99, len = n)

  code(
    fuel_class(r), fuel_cat(sok), anthracite_type(vnudaf), anthracite_subtype(ar)
  )
}
