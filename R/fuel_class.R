#' Determine the class of fossil fuels in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 3})
#' determine the class of fossil fuel (brown, or hard coal, or anthracite) by
#' its reflectance of vitrinite.
#'
#' @param r
#'   reflectance of vitrinite, [\emph{\%}], measured in accordance with
#'   \strong{ISO 7404-5}.
#'   Type: [\code{double}].
#'
#' @return
#'   Identifier of fossil fuel class according to \emph{Table 3} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' fuel_class(c(0.20, 0.52, 1.3, 4.99, 4.998))
#' # [1] "02" "05" "13" "49" "50"
#'
#' # building test:
#' stopifnot(
#'   all(
#'     fuel_class(c(0.20, 0.52, 1.3, 4.99, 4.998)) ==
#'     c("02", "05", "13", "49", "50")
#'   )
#' )
#'
fuel_class <- function(r){
  checkmate::assert_double(r, 0, 7, any.missing = FALSE, min.len = 1)
  r <- round(r, 2)
  sprintf(
    "%02i",
    floor(r * 10)*(r < 5.00) + 50 * (r > 4.99)
  )
}

