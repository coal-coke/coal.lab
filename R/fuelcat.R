#' Determine the category of fossil fuels in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 4})
#' determine the category of fossil fuel by total volume of
#' all \emph{fusinite} fractions. The next \emph{fusinite} fractions may be
#' considered:
#' \describe{
#'   \item{\emph{inertinite}}{determined in accordance with \href{https://www.iso.org/standard/42831.html}{ISO 7404-3}}
#'   \item{two thirds of \emph{semivitrinite}}{determined in accordance with \href{http://docs.cntd.ru/document/1200105478}{GOST R 55662}}
#' }
#'
#' @param sok
#'   total volume of all \emph{fusinite} fractions, [\emph{\%}]. Type: [\code{numeric}].
#'
#' @return
#'   Identifier of fossil fuel category according to \emph{Table 4} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#'   fuelcat(c(5.0, 10, 59, 80))
#'   # [1] "0" "1" "5" "7"

fuelcat <- function(sok){
  checkmate::assert_numeric(sok, 0, 100, any.missing = FALSE, min.len = 1)
  sok <- round(sok)
  sprintf(
    "%i",
    floor(sok * 0.1) * (sok < 70) + 7*(sok > 69)
  )
}

